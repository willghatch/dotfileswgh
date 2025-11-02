#!/usr/bin/env python3

# Mostly vibe-coded replacement for a worse script that I wrote some years ago,
# to give reasonable file names to media files with sequential or otherwise poor
# file names.

import argparse
import os
import subprocess
import sys
from pathlib import Path
from collections import defaultdict
from datetime import datetime

# Supported media types and their exiftool date tags
MEDIA_TYPES = {
    '.jpg': ['EXIF:CreateDate', 'EXIF:DateTimeOriginal', 'QuickTime:CreateDate'],
    '.jpeg': ['EXIF:CreateDate', 'EXIF:DateTimeOriginal', 'QuickTime:CreateDate'],
    '.mp4': ['QuickTime:CreateDate', 'QuickTime:MediaCreateDate', 'QuickTime:TrackCreateDate'],
}


def get_creation_date(filepath):
    """
    Extract creation date from media file using exiftool.
    Returns tuple: (datetime object or None, fractional seconds or None)
    """
    ext = Path(filepath).suffix.lower()
    if ext not in MEDIA_TYPES:
        return None, None

    try:
        # Get all date fields with subseconds
        result = subprocess.run(
            ['exiftool', '-time:all', '-s2', filepath],
            capture_output=True,
            text=True,
            check=False
        )

        if result.returncode != 0:
            return None, None

        lines = result.stdout.strip().split('\n')
        date_tags = MEDIA_TYPES[ext]

        # Try to find the first available date tag
        date_str = None
        subsec = None

        for line in lines:
            if not line:
                continue

            for tag in date_tags:
                tag_name = tag.split(':')[1]
                if line.startswith(tag_name):
                    parts = line.split(':', 1)
                    if len(parts) == 2:
                        date_str = parts[1].strip()
                        break

            # Also check for subsecond time
            if 'SubSec' in line and 'Original' in line:
                parts = line.split(':', 1)
                if len(parts) == 2:
                    subsec = parts[1].strip()

        if not date_str:
            return None, None

        # Check for invalid dates
        if date_str.startswith('0000:00:00') or date_str == '':
            return None, None

        # Parse the date string (format: YYYY:MM:DD HH:MM:SS)
        try:
            dt = datetime.strptime(date_str, '%Y:%m:%d %H:%M:%S')
            return dt, subsec
        except ValueError:
            return None, None

    except FileNotFoundError:
        print("Error: exiftool not found. Please install exiftool.", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error processing {filepath}: {e}", file=sys.stderr)
        return None, None



def collect_files(paths):
    """
    Collect all media files from the given paths.
    If path is a directory, get media files in that directory (non-recursive).
    """
    files = []
    for path_str in paths:
        path = Path(path_str)

        if not path.exists():
            print(f"Warning: {path} does not exist, skipping", file=sys.stderr)
            continue

        if path.is_file():
            if path.suffix.lower() in MEDIA_TYPES:
                files.append(path)
            else:
                print(f"Warning: {path} is not a supported media type, skipping")
        elif path.is_dir():
            # Get all media files in directory (non-recursive)
            for ext in MEDIA_TYPES.keys():
                files.extend(path.glob(f'*{ext}'))
                files.extend(path.glob(f'*{ext.upper()}'))

    return files


def main():
    parser = argparse.ArgumentParser(
        description='Rename media files based on their creation date from EXIF metadata'
    )
    parser.add_argument(
        'paths',
        nargs='+',
        help='Files or directories to process'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be done without making changes'
    )

    args = parser.parse_args()

    # Collect all files to process
    files = collect_files(args.paths)

    if not files:
        print("No media files to process")
        return 0

    # Extract creation dates for all files
    file_dates = {}
    no_metadata_files = []

    for filepath in files:
        dt, subsec = get_creation_date(filepath)
        if dt is None:
            no_metadata_files.append(filepath)
        else:
            file_dates[filepath] = (dt, subsec)

    # Report files without metadata
    if no_metadata_files:
        print("Files without creation metadata (will not be renamed):")
        for f in no_metadata_files:
            print(f"  {f}")
        print()

    if not file_dates:
        print("No files with valid creation dates found")
        return 0

    # Group files by their target base name (date without subseconds)
    # to handle duplicates
    base_groups = defaultdict(list)
    for filepath, (dt, subsec) in file_dates.items():
        base_name = dt.strftime('%Y-%m-%d_%H-%M-%S')
        base_groups[base_name].append((filepath, dt, subsec))

    # Generate new filenames
    renames = {}  # old_path -> new_path

    for base_name, group in base_groups.items():
        if len(group) == 1:
            # Single file with this timestamp
            filepath, dt, subsec = group[0]
            ext = filepath.suffix.lower()
            new_name = f"{base_name}{ext}"
            new_path = filepath.parent / new_name
            renames[filepath] = new_path
        else:
            # Multiple files with same second-level timestamp
            # First, try to disambiguate with subseconds
            subsec_map = {}
            no_subsec = []

            for filepath, dt, subsec in group:
                if subsec:
                    key = f"{base_name}.{subsec}"
                    if key not in subsec_map:
                        subsec_map[key] = []
                    subsec_map[key].append((filepath, dt, subsec))
                else:
                    no_subsec.append((filepath, dt, subsec))

            # Files that were disambiguated by subseconds
            for subsec_key, subsec_files in subsec_map.items():
                if len(subsec_files) == 1:
                    filepath, dt, subsec = subsec_files[0]
                    ext = filepath.suffix.lower()
                    new_name = f"{subsec_key}{ext}"
                    new_path = filepath.parent / new_name
                    renames[filepath] = new_path
                else:
                    # Still duplicates even with subseconds - use letter suffix
                    for idx, (filepath, dt, subsec) in enumerate(sorted(subsec_files, key=lambda x: str(x[0]))):
                        ext = filepath.suffix.lower()
                        suffix = chr(ord('a') + idx)
                        new_name = f"{subsec_key}_{suffix}{ext}"
                        new_path = filepath.parent / new_name
                        renames[filepath] = new_path

            # Files without subseconds - use letter suffix
            if no_subsec:
                for idx, (filepath, dt, subsec) in enumerate(sorted(no_subsec, key=lambda x: str(x[0]))):
                    ext = filepath.suffix.lower()
                    suffix = chr(ord('a') + idx)
                    new_name = f"{base_name}_{suffix}{ext}"
                    new_path = filepath.parent / new_name
                    renames[filepath] = new_path

    # Check for conflicts with existing files
    conflicts = []
    for old_path, new_path in renames.items():
        if new_path.exists():
            # There's an existing file at new_path
            # OK only if: new_path is an input file AND it's staying in place
            if new_path in renames and renames[new_path] == new_path:
                # The existing file is an input that's staying put - no conflict
                continue
            else:
                # Conflict: would overwrite an existing file
                conflicts.append((old_path, new_path))

    if conflicts:
        print("ERROR: The following renames would overwrite existing files:", file=sys.stderr)
        for old_path, new_path in conflicts:
            print(f"  {old_path} -> {new_path} (already exists)", file=sys.stderr)
        print("\nNo changes made. Please resolve conflicts manually and try again.", file=sys.stderr)
        return 1

    # Display or execute renames
    if args.dry_run:
        print("Dry run - no changes will be made:")
        print()

    for old_path, new_path in sorted(renames.items(), key=lambda x: str(x[0])):
        if old_path == new_path:
            continue  # File already has the correct name

        print(f"{old_path} -> {new_path}")

        if not args.dry_run:
            try:
                old_path.rename(new_path)
            except Exception as e:
                print(f"Error renaming {old_path}: {e}", file=sys.stderr)
                return 1

    if args.dry_run:
        print()
        print("Run without --dry-run to apply these changes")
    else:
        print(f"\nSuccessfully renamed {len([r for r in renames.items() if r[0] != r[1]])} files")

    return 0


if __name__ == '__main__':
    sys.exit(main())

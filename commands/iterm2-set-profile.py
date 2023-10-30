#!/usr/bin/env python3

## Usage: <this-script> <profile-name>

import iterm2
import sys

profileName = sys.argv[1]

async def main(connection):
    # Your code goes here. Here's a bit of example code that adds a tab to the current window:
    app = await iterm2.async_get_app(connection)
    # Query for the list of profiles so we can search by name. This returns a
    # subset of the full profiles so it's fast.
    async def getProfileByName(name):
      partialProfiles = await iterm2.PartialProfile.async_query(connection)
      for partial in partialProfiles:
          if partial.name == name:
              full = await partial.async_get_full_profile()
              return full
      return None
    profile = await getProfileByName(profileName)
    if profile != None:
        for window in app.windows:
            for tab in window.tabs:
                for session in tab.sessions:
                    await session.async_set_profile(profile)

iterm2.run_until_complete(main)

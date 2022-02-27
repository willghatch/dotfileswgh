# TODO - this successfully builds, but the resulting executable just crashes...

{
pkgs ? import <nixpkgs> {},
#stdenv,
#fetchurl, fetchgit,
#bash,
#
#sound-theme-freedesktop,
#alsa-lib,
#libpulseaudio,
#
#pkgconfig,
#meson,
#ninja,
#cmake,
#gcc,
#glib,
#libsoup,
#json-glib,
#libevdev,
#
#gst_all_1,
##gst_all_1.gstreamer,
#
#speex,
#speexdsp,
#webrtc-audio-processing,


}:
pkgs.stdenv.mkDerivation rec {
  name = "genie-client";
  src = pkgs.fetchgit {
    url = "https://github.com/stanford-oval/genie-client";
    # Maybe I should get a release tag instead...
    rev = "9a2d2ae96e9ecc8ad1311b018aa754b08d82bac3";
    sha256 = "0i5xjcf8fzp3jf9qfb1crj8a472q77790wrama0mwnbc6qyrx1i4";
  };
  buildInputs = [ 
    pkgs.bash

    pkgs.gcc
    pkgs.cmake
    pkgs.meson
    pkgs.ninja
    pkgs.pkgconfig

    pkgs.alsa-lib
    pkgs.libpulseaudio


    pkgs.glib
    pkgs.libsoup
    pkgs.json-glib
    pkgs.libevdev
    pkgs.speex
    pkgs.speexdsp
    pkgs.webrtc-audio-processing
    
    pkgs.gst_all_1.gstreamer
    pkgs.gst_all_1.gst-plugins-base
    pkgs.gst_all_1.gst-plugins-good

    pkgs.sound-theme-freedesktop
  ];

  builder = "${pkgs.bash}/bin/bash";
  args = [ ./genie-client-builder.sh "${pkgs.sound-theme-freedesktop}" ];
}


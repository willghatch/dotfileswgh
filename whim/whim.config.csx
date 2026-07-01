#nullable enable
#r "WHIM_PATH\Whim.dll"
#r "WHIM_PATH\plugins\Whim.Bar\Whim.Bar.dll"
#r "WHIM_PATH\plugins\Whim.CommandPalette\Whim.CommandPalette.dll"
#r "WHIM_PATH\plugins\Whim.FloatingWindow\Whim.FloatingWindow.dll"
#r "WHIM_PATH\plugins\Whim.FocusIndicator\Whim.FocusIndicator.dll"
#r "WHIM_PATH\plugins\Whim.Gaps\Whim.Gaps.dll"
#r "WHIM_PATH\plugins\Whim.LayoutPreview\Whim.LayoutPreview.dll"
#r "WHIM_PATH\plugins\Whim.SliceLayout\Whim.SliceLayout.dll"
#r "WHIM_PATH\plugins\Whim.TreeLayout\Whim.TreeLayout.dll"
#r "WHIM_PATH\plugins\Whim.TreeLayout.Bar\Whim.TreeLayout.Bar.dll"
#r "WHIM_PATH\plugins\Whim.TreeLayout.CommandPalette\Whim.TreeLayout.CommandPalette.dll"
#r "WHIM_PATH\plugins\Whim.Updater\Whim.Updater.dll"
#r "WHIM_PATH\plugins\Whim.Yaml\Whim.Yaml.dll"

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using Microsoft.UI;
using Microsoft.UI.Xaml.Markup;
using Microsoft.UI.Xaml.Media;
using Whim;
using Whim.Bar;
using Whim.CommandPalette;
using Whim.FloatingWindow;
using Whim.FocusIndicator;
using Whim.Gaps;
using Whim.LayoutPreview;
using Whim.SliceLayout;
using Whim.TreeLayout;
using Whim.TreeLayout.Bar;
using Whim.TreeLayout.CommandPalette;
using Whim.Updater;
using Whim.Yaml;
using Windows.Win32.UI.Input.KeyboardAndMouse;

void DoConfig(IContext context)
{
	context.Logger.Config = new LoggerConfig();

	context.CommandManager.Add(
		identifier: "whim.custom.lock_screen",
		title: "Lock screen",
		callback: () => Process.Start("rundll32.exe", "user32.dll,LockWorkStation")
	);
	context.KeybindManager.SetKeybind("whim.custom.lock_screen", new Keybind(KeyModifiers.LWin, VIRTUAL_KEY.VK_Q));

	// YAML config. It's best to load this first so that you can use it in your C# config.
	YamlLoader.Load(context);
}

// We return doConfig here so that Whim can call it when it loads.
return DoConfig;

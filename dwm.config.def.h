/* See LICENSE file for copyright and license details. */


// My const and function definitions

#define MYNUMTAGS 9
#define MYTAGMASK ((1 << MYNUMTAGS) -1)

void
viewNext(const Arg *arg) {
        // Go to tag to the right if arg->ui nonzero
        unsigned int a = arg->ui;
        int newtags = selmon->tagset[selmon->seltags];
        newtags = a ? newtags <<1 : newtags >>1;
	if(!(newtags & MYTAGMASK)) {
	    newtags = a ? 1 : (1<<MYNUMTAGS)>>1;
	}
	//selmon->seltags ^= 1;
        selmon->tagset[selmon->seltags] = newtags;
	focus(NULL);
	arrange(selmon);
}

void
tagNext(const Arg *arg) {
        // Move client to next tag if arg->ui nonzero
        unsigned int a = arg->ui;
        int newtags = selmon->tagset[selmon->seltags];
        newtags = a ? newtags <<1 : newtags >>1;
	if(!(newtags & MYTAGMASK)) {
	    newtags = a ? 1 : (1<<MYNUMTAGS)>>1;
	}
	selmon->sel->tags = newtags;

        focus(NULL);
        arrange(selmon);
}

void
tagAndViewNext(const Arg *arg) {
        tagNext(arg);
        viewNext(arg);
}

void
tagAndFocusMon(const Arg *arg) {
        tagmon(arg);
        focusmon(arg);
}





/* appearance */
static const char font[]            = "-*-terminus-medium-r-*-*-16-*-*-*-*-*-*-*";
static const char normbordercolor[] = "#444444";
static const char normbgcolor[]     = "#222222";
static const char normfgcolor[]     = "#bbbbbb";
static const char selbordercolor[]  = "#005577";
static const char selbgcolor[]      = "#005577";
static const char selfgcolor[]      = "#eeeeee";
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const Bool showbar           = True;     /* False means no bar */
static const Bool topbar            = True;     /* False means bottom bar */

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
	/* class      instance    title       tags mask     isfloating   monitor */
	//{ "Gimp",     NULL,       NULL,       0,            True,        -1 },
	{ "bashrun",     NULL,       NULL,       0,            True,        -1 },
	//{ "Firefox",  NULL,       NULL,       1 << 8,       False,       -1 },
};

/* layout(s) */
static const float mfact      = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster      = 1;    /* number of clients in master area */
static const Bool resizehints = True; /* True means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
};

/* key definitions */
#define SUPKEY Mod4Mask
#define HYPKEY Mod2Mask
#define ALTKEY Mod1Mask
#define CTRLKEY ControlMask
// I don't even use this thing, but I'll leave it in case
#define TAGKEYS(KEY,TAG) \
	{ HYPKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ HYPKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ HYPKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ HYPKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static const char *dmenucmd[] = { "dmenu_run", "-fn", font, "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbgcolor, "-sf", selfgcolor, NULL };
static const char *termcmd[]  = { "gnome-terminal", NULL };
static const char *bashruncmd[]  = { "bashrun", NULL };

static Key keys[] = {
	/* modifier                     key        function        argument */
	{ HYPKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
	{ HYPKEY,                       XK_Return, spawn,          {.v = termcmd } },
	{ HYPKEY|SUPKEY,                XK_Return, spawn,          {.v = bashruncmd } },
	//{ HYPKEY,                       XK_b,      togglebar,      {0} },
	{ HYPKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ HYPKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ HYPKEY|SUPKEY,                XK_h,      incnmaster,     {.i = +1 } },
	{ HYPKEY|SUPKEY,                XK_l,      incnmaster,     {.i = -1 } },
	{ HYPKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ HYPKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	//{ HYPKEY,                       XK_Return, zoom,           {0} },
	//{ HYPKEY,                       XK_Tab,    view,           {0} },
	{ HYPKEY,                       XK_c,      killclient,     {0} },
	{ HYPKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ HYPKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ HYPKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ HYPKEY,                       XK_space,  setlayout,      {0} },
	{ HYPKEY|ShiftMask,             XK_space,  togglefloating, {0} },
	//{ HYPKEY,                       XK_0,      view,           {.ui = ~0 } },
	//{ HYPKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ HYPKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ HYPKEY,                       XK_period, focusmon,       {.i = +1 } },
	//{ HYPKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	//{ HYPKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
	{ HYPKEY|SUPKEY,             XK_comma,  tagAndFocusMon, {.i = -1 } },
	{ HYPKEY|SUPKEY,             XK_period, tagAndFocusMon, {.i = +1 } },
	{ HYPKEY,                       XK_o,      viewNext,       {.ui = 0 } },
	{ HYPKEY,                       XK_e,      viewNext,       {.ui = 1 } },
	{ HYPKEY|SUPKEY,                XK_o,      tagAndViewNext, {.ui = 0 } },
	{ HYPKEY|SUPKEY,                XK_e,      tagAndViewNext, {.ui = 1 } },
	//TAGKEYS(                        XK_1,                      0)
	//TAGKEYS(                        XK_2,                      1)
	//TAGKEYS(                        XK_3,                      2)
	//TAGKEYS(                        XK_4,                      3)
	//TAGKEYS(                        XK_5,                      4)
	//TAGKEYS(                        XK_6,                      5)
	//TAGKEYS(                        XK_7,                      6)
	//TAGKEYS(                        XK_8,                      7)
	//TAGKEYS(                        XK_9,                      8)
	{ HYPKEY|ShiftMask,             XK_q,      quit,           {0} },
};

/* button definitions */
/* click can be ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         HYPKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         HYPKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         HYPKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            HYPKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            HYPKEY,         Button3,        toggletag,      {0} },
};


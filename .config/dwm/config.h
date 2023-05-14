/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 2;       /* border pixel of windows */
static const unsigned int gappx     = 6;        /* gaps between windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int swallowfloating    = 0;        /* 1 means swallow floating windows by default */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const int user_bh            = 6;        /* 2 is the default spacing around the bar's font */
static const char *fonts[]          = { "LiterationMono Nerd Font Mono:style=Book:size=10",
                                          "FZHei-B01T,方正黑体繁体:style=Regular:size=11"  };
static const char col_bg_desel[]       = "#222222"; 
static const char col_bg_tag[]        = "#f4d47c";
static const char col_bg_info[]        = "#2069e0";
static const char col_border_desel[]       = "#333333";
static const char col_border_sel[]       = "#fcc732";
static const char col_text_sel[]       = "#333333"; 
static const char col_text_desel[]        = "#bbbbbb"; 
static const unsigned int lowbaralpha = 0xd4;
static const unsigned int baralpha = 0x90;
static const unsigned int op = OPAQUE;
static const char *colors[][3]      = {
	/*                      fg                  bg         border   */
        [SchemeNorm] =      { col_text_desel,     "#000000",      col_border_desel },
	[SchemeSel]  =      { col_text_sel,       "#000000",  col_border_sel  },
	[SchemeHid] =       { col_text_desel,     col_bg_desel,      col_border_sel}, 
	[SchemeStatus]  =   { col_text_desel,     col_bg_desel,      "#000000"  }, // Statusbar right {text,background,not used but cannot be empty}
	[SchemeTagsSel]  =  { col_text_sel,       col_bg_tag,   "#000000"  }, // Tagbar left selected {text,background,not used but cannot be empty}
	[SchemeTagsNorm]  = { col_text_desel,     col_bg_desel,      "#000000"  }, // Tagbar left unselected {text,background,not used but cannot be empty}
	[SchemeInfoSel]  =  { "#222222",       col_bg_info,   "#000000"  }, // infobar middle  selected {text,background,not used but cannot be empty}
	[SchemeInfoNorm]  = { col_text_desel,     col_bg_desel,       "#000000"  }, // infobar middle  unselected {text,background,not used but cannot be empty}
};

static const unsigned int alphas[][3]      = {
	/*               fg      bg        border     */
	[SchemeNorm] = { op, op, op },
	[SchemeSel]  = { op, op, op },
       	[SchemeHid] = { op, op, op },
	[SchemeStatus] = { op, lowbaralpha, op },
	[SchemeTagsSel] = { op, lowbaralpha, op },
	[SchemeTagsNorm] = { op, baralpha, op },
	[SchemeInfoSel] = { op, lowbaralpha, op },
	[SchemeInfoNorm] = { op, baralpha, op },

};

/* tagging */
static const char *tags[] = { "一", "二", "三", "四", "五", "六", "七", "八", "九" };

// this is supposedly for swallowing but it doesnt work.
static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class    instance       title           tags mask  isfloating  isterminal  noswallow  monitor */
	{ "Alacritty" , NULL,     NULL,           0,         0,          1,           0,        -1 },
	{ NULL,      NULL,     "Event Tester",        0,         0,          0,           1,      -1 }, /* xev */
};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1; /* 1 will force focus on the fullscreen window */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
};

/* key definitions */
#define MODKEY Mod1Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run", NULL};
static const char *termcmd[]  = { "alacritty", NULL };
static const char *emacscmd[] = { "emacsclient", "-c", "-a", "emacs", NULL};
static const char *browsercmd[] = { "qutebrowser", NULL };
static const char *dmconfcmd[] = {"/home/cruise/.dmscripts/dmconf", NULL};

static const Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY,                       XK_space,  spawn,          {.v = emacscmd} },
	{ MODKEY,                       XK_f,      spawn,          {.v = browsercmd } },
	{ MODKEY,                       XK_c,      spawn,          {.v = dmconfcmd} },
	{ MODKEY,                       XK_j,      focusstackvis,  {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstackvis,  {.i = -1 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_Return, zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
	{ MODKEY,                       XK_s,      showall,           {0} },
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)
	{ MODKEY|ShiftMask,             XK_q,      quit,           {0} },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static const Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button1,        togglewin,      {0} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

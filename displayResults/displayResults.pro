% Copyright Prolog Development Center

implement displayResults inherits dialog
    open core, vpiDomains

clauses
    getNumber(Parent, Number, WinTurns) :-
        Dialog = new(Parent),
        Dialog:show(),
        Dialog:getVals(Number, WinTurns).

clauses
    new(Parent) :-
        dialog::new(Parent),
        generatedInitialize().

clauses
    getVals(Number, WinTurns) :-
        Number = number_ctl:getInteger(),
        WinTurns = winturn_ctl:getInteger().
% This code is maintained automatically, do not update it manually.
%  18:31:02-22.5.2020

facts
    ok_ctl : button.
    cancel_ctl : button.
    help_ctl : button.
    number_ctl : integercontrol.
    winturn_ctl : integercontrol.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("displayResults"),
        setRect(rct(50, 40, 290, 160)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        ok_ctl := button::newOk(This),
        ok_ctl:setText("&OK"),
        ok_ctl:setPosition(48, 98),
        ok_ctl:setSize(56, 16),
        ok_ctl:defaultHeight := false,
        ok_ctl:setAnchors([control::right, control::bottom]),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("Cancel"),
        cancel_ctl:setPosition(112, 98),
        cancel_ctl:setSize(56, 16),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        help_ctl := button::new(This),
        help_ctl:setText("&Help"),
        help_ctl:setPosition(176, 98),
        help_ctl:setSize(56, 16),
        help_ctl:defaultHeight := false,
        help_ctl:setAnchors([control::right, control::bottom]),
        StaticText_ctl = textControl::new(This),
        StaticText_ctl:setText("Wins by Turn:"),
        StaticText_ctl:setPosition(20, 38),
        StaticText1_ctl = textControl::new(This),
        StaticText1_ctl:setText("Number:"),
        StaticText1_ctl:setPosition(20, 22),
        number_ctl := integercontrol::new(This),
        number_ctl:setPosition(72, 20),
        number_ctl:setAutoHScroll(false),
        number_ctl:setAlignBaseline(false),
        winturn_ctl := integercontrol::new(This),
        winturn_ctl:setPosition(72, 36),
        winturn_ctl:setAutoHScroll(false),
        winturn_ctl:setAlignBaseline(false).
% end of automatic code

end implement displayResults

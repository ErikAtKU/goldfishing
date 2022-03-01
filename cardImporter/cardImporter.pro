% Copyright Prolog Development Center

implement cardImporter inherits dialog
    open core, vpiDomains, rules

clauses
    display(Parent) = Dialog :-
        Dialog = new(Parent),
        Dialog:show().

clauses
    new(Parent) :-
        dialog::new(Parent),
        generatedInitialize().

predicates
    onCancelClick : button::clickResponder.
clauses
    onCancelClick(_Source) = button::defaultAction.

predicates
    onModify : editControl::modifiedListener.
clauses
    onModify(_Source) :-
        FullInput = string::split(input_ctl:getText(), "\n"),
        Name = parseName_dt(FullInput),
        FactName = makeFactName_dt(Name),
        Type = parseType_dt(FullInput),
        ManaCost = [ Mana || Mana = parseManaCost_nd(FullInput) ],
        MakeLine1 = string::format("%s : card := erroneous.", FactName),
        if creature = Type then
            tuple(P, T) = parsePT_dt(FullInput),
            MakeLine2 = string::format("%s := card::%s(\"%s\", %s, %d, %d),", FactName, toString(Type), Name, toString(ManaCost), P, T)
        else
            MakeLine2 = string::format("%s := card::%s(\"%s\", %s),", FactName, toString(Type), Name, toString(ManaCost))
        end if,
        output_ctl:setText(string::concatWithDelimiter([MakeLine1, MakeLine2], "\n")),
        !.
    onModify(_Source) :-
        output_ctl:setText("Cannot parse.").

predicates
    parseType_dt : (string*) -> cardType determ.
clauses
    parseType_dt([Line | _]) = creature :-
        _ = string::search(Line, "Creature —", string::caseSensitive),
        !.
    parseType_dt([Line | _]) = instant :-
        _ = string::search(Line, "Instant —", string::caseSensitive),
        !.
    parseType_dt([Line | _]) = sorcery :-
        _ = string::search(Line, "Sorcery —", string::caseSensitive),
        !.
    parseType_dt([Line | _]) = land :-
        _ = string::search(Line, "Land —", string::caseSensitive),
        !.
    parseType_dt([Line | _]) = instant :-
        _ = string::search(Line, "Instant", string::caseSensitive),
        !.
    parseType_dt([Line | _]) = sorcery :-
        _ = string::search(Line, "Sorcery", string::caseSensitive),
        !.
    parseType_dt([Line | _]) = land :-
        _ = string::search(Line, "Land", string::caseSensitive),
        !.
    parseType_dt([_Line | Tail]) = parseType_dt(Tail).

predicates
    parseManaCost_nd : (string*) -> mana nondeterm.
clauses
    parseManaCost_nd([Line | _]) = Mana :-
        SplitLine = string::split(Line, "{"),
        ManaCosts = list::filteredMap(SplitLine, parseManaCost_dt),
        [] <> ManaCosts,
        !,
        Mana in ManaCosts.
    parseManaCost_nd([_ | Tail]) = parseManaCost_nd(Tail).

predicates
    parsePT_dt : (string*) -> tuple{integer, integer} determ.
clauses
    parsePT_dt([Line | _]) = tuple(P, T) :-
        SplitLine = string::split(Line, "/"),
        [PStr, TStr] = SplitLine,
        P = tryToTerm(PStr),
        T = tryToTerm(TStr),
        !.
    parsePT_dt([_ | Tail]) = parsePT_dt(Tail).

predicates
    parseManaCost_dt : (string) -> mana determ.
clauses
    parseManaCost_dt(String) = _ :-
        not(_ = string::search(String, "}")),
        !,
        fail.
    parseManaCost_dt(String) = h(First, Second) :-
        SplitList = string::split(String, "/"),
        [One, Two | _] = SplitList,
        First = parseManaCost_dt(string::concat(One, "}")),
        Second = parseManaCost_dt(Two),
        !.
    parseManaCost_dt("R}") = r :-
        !.
    parseManaCost_dt("U}") = u :-
        !.
    parseManaCost_dt("G}") = g :-
        !.
    parseManaCost_dt("B}") = b :-
        !.
    parseManaCost_dt("W}") = w :-
        !.
    parseManaCost_dt(String) = i(Val) :-
        Generic = string::replaceAll(String, "}", ""),
        Val = tryToTerm(Generic),
        !.
    parseManaCost_dt(String) = Mana :-
        List = string::split(string::trim(String), "}"),
        FilterList = list::filter(List, { (Elem) :- not(Elem = "") }),
        list::length(FilterList) > 1,
        PotentialMana in FilterList, %+
            Mana = parseManaCost_dt(string::concat(PotentialMana, "}")),
        !.

predicates
    parseName_dt : (string*) -> string determ.
clauses
    parseName_dt([First | _]) = Name :-
        FirstLineList = string::split(First, "{"),
        [PresumedName | _Rest] = FirstLineList,
        Name = string::trim(PresumedName).

predicates
    makeFactName_dt : (string) -> string determ.
clauses
    makeFactName_dt(NameStr) = FactName :-
        NameList = string::split(NameStr, " "),
        UpperFactName =
            list::foldl_dt(NameList,
                { (Name, Elem) = Result :-
                    string::tryFront(Elem, 1, First, Rest),
                    FirstLetter = string::toUpperCase(First),
                    RestWord = string::toLowerCase(Rest),
                    Result = string::format("%s%s%s", Name, FirstLetter, RestWord)
                },
                ""),
        string::tryFront(UpperFactName, 1, First, Rest),
        FirstLetter = string::toLowerCase(First),
        FactName = string::format("%s%s", FirstLetter, Rest).

% This code is maintained automatically, do not update it manually.%  15:02:26-30.7.2020
facts
    cancel_ctl : button.
    input_ctl : editControl.
    output_ctl : editControl.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("cardImporter"),
        setRect(rct(50, 40, 290, 286)),
        setModal(true),
        setDecoration(titlebar([closeButton])),
        setState([wsf_NoClipSiblings]),
        cancel_ctl := button::newCancel(This),
        cancel_ctl:setText("Close"),
        cancel_ctl:setPosition(176, 226),
        cancel_ctl:setSize(56, 16),
        cancel_ctl:defaultHeight := false,
        cancel_ctl:setAnchors([control::right, control::bottom]),
        cancel_ctl:setClickResponder(onCancelClick),
        input_ctl := editControl::new(This),
        input_ctl:setText(""),
        input_ctl:setPosition(8, 6),
        input_ctl:setWidth(224),
        input_ctl:setHeight(102),
        input_ctl:setMultiLine(),
        input_ctl:setWantReturn(),
        input_ctl:addModifiedListener(onModify),
        output_ctl := editControl::new(This),
        output_ctl:setText("texts"),
        output_ctl:setPosition(8, 116),
        output_ctl:setWidth(224),
        output_ctl:setHeight(102),
        output_ctl:setMultiLine(),
        output_ctl:setWantReturn(),
        output_ctl:setVScroll(),
        output_ctl:setHScroll(),
        output_ctl:setReadOnly().
% end of automatic code

end implement cardImporter

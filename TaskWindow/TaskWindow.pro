% Copyright Prolog Development Center

implement taskWindow inherits applicationWindow
    open core, vpiDomains

facts
    stream : outputStream := erroneous.
    playerResultMap : mapM{string, mapM{integer, string*}} := mapM_redBlack::new().
    winConResultMap : mapM{string, mapM{string, tuple{integer, integer, string}}} := mapM_redBlack::new().
    playerMatches : mapM{string, integer} := mapM_redBlack::new().
    critical : criticalSection := criticalSection::new().

constants
    mdiProperty : boolean = true.

clauses
    new() :-
        applicationWindow::new(),
        generatedInitialize().

predicates
    onShow : window::showListener.
clauses
    onShow(_, _CreationData) :-
        MessageForm = messageForm::display(This),
        FrameWin = MessageForm:frameWindow,
        vpi::winSetState(FrameWin:getVpiWindow(), [vpiDomains::wsf_Maximized]),
        stream := MessageForm:getOutputStream().

predicates
    runContest : (player Player1, player Player2, integer Matches).
clauses
    runContest(Player1, Player2, Matches) :-
        try
            foreach _ = std::fromTo(1, Matches) do
                Match = match::new(Player1, Player2),
                Match:getResults(Winner, Turn, Reason, Boardstate),
                critical:enter(),
                processWin(Winner, Turn, Reason, Boardstate),
                critical:leave()
            end foreach
        catch E do
            stream:write(E)
        end try.

predicates
    processWin : (string Player, integer Turn, string Reason, string Boardstate).
clauses
    processWin(WinnerName, Turn, Reason, Boardstate) :-
        WinTrim = string::trim(WinnerName),
        if OtherWinner in winConResultMap:keyList and string::equalIgnoreCase(WinTrim, OtherWinner) then
            Winner = OtherWinner
        else
            Winner = WinTrim
        end if,
        ResultMap = playerResultMap:get_default(Winner, mapM_redBlack::new()),
        List = ResultMap:get_default(Turn, []),
        ResultMap:set(Turn, [Reason | List]),
        WinConMap = winConResultMap:get_default(Winner, mapM_redBlack::new()),
        tuple(Count, LowestTurn, PrevBoardstate) = WinConMap:get_default(Reason, tuple(0, 60, "")),
        if Turn <= 2 then
            stream:writef("[%02d] , %s || %s\r\n", Turn, Reason, Boardstate)
        end if,
        if Turn < LowestTurn then
            NewTurn = Turn,
            NewBoardState = Boardstate
        elseif Turn = LowestTurn and string::length(Boardstate) < string::length(PrevBoardstate) then
            NewTurn = Turn,
            NewBoardState = Boardstate
        else
            NewTurn = LowestTurn,
            NewBoardState = PrevBoardstate
        end if,
        WinConMap:set(Reason, tuple(Count + 1, NewTurn, NewBoardState)),
        CurrentPMatches = playerMatches:get_default(Winner, 0),
        playerMatches:set(Winner, 1 + CurrentPMatches).

predicates
    printTop : (integer Num, integer ByTurn).
clauses
    printTop(Num, Turn) :-
        setText("goldfishing"),
        stream:writef("Top {%d} wins by Turn <%d>\r\n", Num, Turn - 1),
        TopMap = mapM_redBlack::newCustom({ (A, B) = compare(B, A) }),
        foreach PlayerName in playerResultMap:keyList do
            ResultMap = playerResultMap:get(PlayerName),
            Count = varM_integer::new(0),
            foreach Key in ResultMap:keyList and Key < Turn do
                TurnList = ResultMap:get(Key),
                NumTurnWins = list::length(TurnList),
                Count:add(NumTurnWins)
            end foreach,
            PlayerList = TopMap:get_default(Count:value, []),
            TopMap:set(Count:value, [PlayerName | PlayerList])
        end foreach,
        First4 = list::take(convert(positive, Num), TopMap:keyList),
        Counter = varM_integer::new(0),
        foreach CountKey in First4 and PlayerName in TopMap:get(CountKey) and Counter:inc() and Counter:value <= Num do
            P1ResultMap = playerResultMap:get(PlayerName),
            P1WinConMap = winConResultMap:get(PlayerName),
            P1MatchesAmount = playerMatches:get(PlayerName),
            stream:write(string::format("%s\r\n", PlayerName)),
            printTree(P1ResultMap, P1MatchesAmount, stream),
            printWinCons(P1WinConMap, P1MatchesAmount, stream)
        end foreach,
        stream:nl().

clauses
    run(Player1, Player2, Matches) :-
        setText(Player1:name),
        CurrentP1Matches = playerMatches:get_default(Player1:name, 0),
        playerMatches:set(Player1:name, Matches + CurrentP1Matches),
        CurrentP2Matches = playerMatches:get_default(Player2:name, 0),
        playerMatches:set(Player2:name, Matches + CurrentP2Matches),
        foreach _ = std::fromTo(1, Matches) do
            Match = match::new(Player1, Player2),
            Match:getResults(Winner, Turn, Reason, Boardstate),
            ResultMap = playerResultMap:get_default(Winner, mapM_redBlack::new()),
            List = ResultMap:get_default(Turn, []),
            ResultMap:set(Turn, [Reason | List]),
            WinConMap = winConResultMap:get_default(Winner, mapM_redBlack::new()),
            tuple(Count, LowestTurn, PrevBoardstate) = WinConMap:get_default(Reason, tuple(0, 60, "")),
            if Turn <= 5 then
                stream:writef("[%02d] , %s || %s\r\n", Turn, Reason, Boardstate)
            end if,
            if Turn < LowestTurn then
                NewTurn = Turn,
                NewBoardState = Boardstate
            elseif Turn = LowestTurn and string::length(Boardstate) < string::length(PrevBoardstate) then
                NewTurn = Turn,
                NewBoardState = Boardstate
            else
                NewTurn = LowestTurn,
                NewBoardState = PrevBoardstate
            end if,
            WinConMap:set(Reason, tuple(Count + 1, NewTurn, NewBoardState))
        end foreach,
        if list::isMember(Player1:name, playerResultMap:keyList) then
            P1ResultMap = playerResultMap:get(Player1:name),
            P1WinConMap = winConResultMap:get(Player1:name),
            P1MatchesAmount = playerMatches:get(Player1:name),
            stream:write(string::format("%s:\r\n", Player1:name)),
            printTree(P1ResultMap, P1MatchesAmount, stream),
            printWinCons(P1WinConMap, P1MatchesAmount, stream)
        end if,
        if list::isMember(Player2:name, playerResultMap:keyList) then
            P2ResultMap = playerResultMap:get(Player2:name),
            P2WinConMap = winConResultMap:get(Player2:name),
            P2MatchesAmount = playerMatches:get(Player2:name),
            stream:write(string::format("%s:\r\n", Player2:name)),
            printTree(P2ResultMap, P2MatchesAmount, stream),
            printWinCons(P2WinConMap, P2MatchesAmount, stream)
        end if,
        stream:flush().

predicates
    printTree : (mapM{integer, string*}, integer Matches, outputStream).
clauses
    printTree(ResultMap, Matches, Stream) :-
        Count = varM_integer::new(0),
        MatchesReal = convert(real, Matches),
        foreach Key in ResultMap:keyList and Key < 7 do
            TurnList = ResultMap:get(Key),
            NumTurnWins = list::length(TurnList),
            Count:add(NumTurnWins),
            %NumTurnWinsReal = convert(real, NumTurnWins),
            NumTurnWinsReal = convert(real, Count:value),
            Winrate = 100 * NumTurnWinsReal / MatchesReal,
            Stream:writef("Turn %02d wins: %06d / %06d / %d - %04.2%s\r\n", Key, NumTurnWins, Count:value, Matches, Winrate, "%")
        end foreach,
        Stream:nl().

predicates
    printWinCons : (mapM{string, tuple{integer, integer, string}}, integer Matches, outputStream).
clauses
    printWinCons(WinConMap, Matches, Stream) :-
        MatchesReal = convert(real, Matches),
        CardWinMap = mapM_redBlack::new(),
        Add0 = varM_boolean::new(false),
        foreach Wincon in WinConMap:keyList do
            tuple(Wins, LowestTurn, Boardstate) = WinConMap:get(Wincon),
            if LowestTurn >= 10 then
                Add0:setTrue()
            end if,
            NoLefts = string::replaceAll(WinCon, "[", "", string::caseInsensitive),
            NoRights = string::replaceAll(NoLefts, "]", "", string::caseInsensitive),
            NoSpace = string::replaceAll(NoRights, " ", "", string::caseInsensitive),
            WinCardList = string::split_delimiter(NoSpace, "-"),
            if [Play, Hand, Grave | _] = WinCardList then
                AllCardsString = string::concatWithDelimiter([Play, Hand, Grave], ","),
                AllCardsList = string::split_delimiter(AllCardsString, ","),
                SingleCards = list::removeDuplicates(AllCardsList),
                foreach Card in SingleCards and not(Card = "") do
                    CardWins = CardWinMap:get_default(Card, 0),
                    CardWinMap:set(Card, CardWins + Wins)
                end foreach
            end if,
            Winrate = 100 * Wins / MatchesReal,
            if Add0:isTrue() then
                Stream:writef("%06d: %05.2%s - [%02d] , %s || %s\r\n", Wins, Winrate, "%", LowestTurn, WinCon, Boardstate)
            else
                Stream:writef("%06d: %05.2%s - [%d] , %s || %s\r\n", Wins, Winrate, "%", LowestTurn, WinCon, Boardstate)
            end if
        end foreach,
        Stream:nl(),
        CardOrdered = mapM_redBlack::new(),
        foreach Card in CardWinMap:keyList do
            Wins = CardWinMap:get(Card),
            List = CardOrdered:get_default(Wins, []),
            CardOrdered:set(Wins, [Card | List])
        end foreach,
        foreach Wins in CardOrdered:keyList and Card in CardOrdered:get(Wins) do
            Winrate = 100 * Wins / MatchesReal,
            Stream:writef("%06d: %05.1%s - %s\r\n", Wins, Winrate, "%", Card)
        end foreach,
        Stream:nl().

predicates
    getCount_nd : (tuple{integer, integer}* MinMaxListIn, integer OpenSlots, integer RemainingMax, tuple{integer, integer}* MinMaxList [out],
        integer Count [out], integer OpenSlotsLeft [out], integer NewRemainingMax [out]) nondeterm.
clauses
    getCount_nd([tuple(Min, Max) | MinMaxList], OpenSlot, RemainingMax, MinMaxList, Count, NewOpenSlot - Count, NewRemainingMax) :-
        NewRemainingMax = RemainingMax - Max,
        NewOpenSlot = OpenSlot + Min,
        SetMin = math::max(Min, NewOpenSlot - NewRemainingMax),
        SetMax = math::min(Max, NewOpenSlot),
        Count = std::fromTo(SetMin, SetMax).

predicates
    getCountList_nd : (tuple{integer, integer}* MinMaxListIn, integer OpenSlots, integer RemainingMax) -> integer* CountList nondeterm.
clauses
    getCountList_nd([], _OpenSlots, 0) = [] :-
        !.
    getCountList_nd([], _OpenSlots, _RemainingMax) = _ :-
        !,
        fail.
    getCountList_nd(MinMaxListIn, OpenSlots, RemainingMax) = List :-
        getCount_nd(MinMaxListIn, OpenSlots, RemainingMax, MinMaxList1, Count, OpenSlot1, RemainingMax1),
        SubList = getCountList_nd(MinMaxList1, OpenSlot1, RemainingMax1),
        List = [Count | SubList].

predicates
    getStartMinMax : (tuple{integer, integer}* MinMaxList, integer Deckslots) -> tuple{integer, integer} RemainingMinMax.
clauses
    getStartMinMax(MinMaxList, Deckslots) =
        list::fold(MinMaxList, { (tuple(Min, Max), tuple(VarMin, VarMax)) = tuple(VarMin - Min, VarMax + Max) }, tuple(Deckslots, 0)).

class predicates
    onDestroy : window::destroyListener.
clauses
    onDestroy(_).

class predicates
    onHelpAbout : window::menuItemListener.
clauses
    onHelpAbout(TaskWin, _MenuTag) :-
        _ = cardImporter::display(TaskWin).

predicates
    onSizeChanged : window::sizeListener.
clauses
    onSizeChanged(_) :-
        vpiToolbar::resize(getVPIWindow()).

predicates
    onFileNew : window::menuItemListener.
clauses
    onFileNew(_Source, _MenuTag) :-
        Deck = deck::myIzzetDeck(),
        foreach WinLine = Deck:showIzzetWinCons_nd() do
            stream:write(WinLine),
            stream:nl()
        end foreach.

class facts
    activeThreads : thread* := [].

predicates
    onFileOpen : window::menuItemListener.
clauses
    onFileOpen(_Source, _MenuTag) :-
        Goldfish = player::goldfish(60),
        Matches = 500,
        NameVar = varM::new(""),
        Processed = varM_integer::new(0),
        Total = varM_integer::new(0),
        activeThreads := [],
        foreach Const = std::fromTo(1, 1) do
            MinMaxList =
                [
                    %Mountain
                    tuple(0, 0),
                    %Island
                    tuple(0, 0),
                    %Aether
                    tuple(10, 40),
                    %Frontier
                    tuple(0, 0),
                    %Izzet
                    tuple(1, 1),
                    %Nivix
                    tuple(2, 2),
                    %League
                    tuple(4, 4),
                    %Manamorphose
                    tuple(1, 1),
                    %Lava
                    tuple(2, 2),
                    %Desperate
                    tuple(1, 1),
                    %Eye
                    tuple(4, 4),
                    %Song
                    tuple(4, 4),
                    %Pup
                    tuple(1, 1),
                    %Ray
                    tuple(1, 1),
                    %Opt
                    tuple(0, 0),
                    %Simian
                    tuple(4, 4),
                    %Biomancer
                    tuple(2, 2),
                    %Zirda
                    tuple(0, 0),
                    %Toils
                    tuple(2, 2),
                    %Dual
                    tuple(2, 2)
                ],
            tuple(RemainingMin, RemainingMax) = getStartMinMax(MinMaxList, 60),
            RunStack = varM::new([]),
            foreach
                [
                        Mountain,
                        Island,
                        Aether,
                        Frontier,
                        Izzet,
                        Nivix,
                        League,
                        Mana,
                        Lava,
                        Desperate,
                        Eye,
                        Song,
                        Pup,
                        Ray,
                        Opt,
                        Simian,
                        Biomancer,
                        Zirda,
                        Toils,
                        Dual |
                        _
                    ]
                    = getCountList_nd(MinMaxList, RemainingMin, RemainingMax)
            do
                Deck =
                    deck::izzetZirdaFullVariable(Mountain, Island, Aether, Frontier, Izzet, Nivix, League, Mana, Lava, Desperate, Eye, Song, Pup,
                        Ray, Opt, Simian, Biomancer, Zirda, Toils, Dual, true),
                Player = player::new(Deck, Deck:getDecklist()),
                Anon =
                    {  :-
                        Thread = thread::start({  :- runContest(Player, Goldfish, Matches) }),
                        activeThreads := [Thread | activeThreads],
                        NameVar:value := Deck:shortName
                    },
                RunStack:value := [Anon | RunStack:value],
                Total:inc()
            end foreach,
            foreach Anon in RunStack:value do
                Anon()
                %if Matches * list::length(activeThreads) > 60000 then
                % synchronize(NameVar:value, Processed, Total:value)
                %end if
            end foreach
        end foreach,
        synchronize(NameVar:value, Processed, Total:value),
        printTop(1, 7).

predicates
    synchronize : (string Name, varM_integer Processed, integer Total).
clauses
    synchronize(Name, Processed, Total) :-
        Length = varM_integer::new(list::length(activeThreads)),
        Title1 = string::format("%s - [%d Threads] - [%d Processed / %d Total]", Name, Length:value, Processed:value, Total),
        setText(Title1),
        Anon1 =
            { (Thread) :-
                Dots = varM::new(""),
                std::repeat(),
                Dots:value := string::format(".%s", Dots:value),
                if string::length(Dots:value) > 5 then
                    Dots:value := ""
                end if,
                Title2 = string::format("%s - [%d Threads] - [%d Processed / %d Total] %s", Name, Length:value, Processed:value, Total, Dots:value),
                setText(Title2),
                if Thread:tryWait(500) then
                    !
                elseif not(multiThread_native::still_active = Thread:exitCode) then
                    !
                else
                    fail
                end if
            },
        foreach T in activeThreads and Anon1(T) do
            Processed:inc(),
            Length:dec()
        end foreach,
        activeThreads := [].

predicates
    onFileSave : window::menuItemListener.
clauses
    onFileSave(_Source, _MenuTag) :-
        displayResults::getNumber(This, Number, WinTurns),
        printTop(Number, WinTurns + 1).

predicates
    onEditCut : window::menuItemListener.
clauses
    onEditCut(_Source, _MenuTag) :-
        foreach _ = std::fromTo(1, 100) do
            stream:nl()
        end foreach.

predicates
    onEditUndo : window::menuItemListener.
clauses
    onEditUndo(_Source, _MenuTag) :-
        Deck1 = deck::electroArclight(),
        Deck2 = deck::quickArclight(),
        Matches = 500,
        activeThreads := [],
        Anon = {  :- runStack(100, Matches, Deck1, Deck2) },
        _ = thread::start(Anon).

predicates
    onEditRedo : window::menuItemListener.
clauses
    onEditRedo(_Source, _MenuTag) :-
        Deck1 = deck::electroArclight(),
        Deck2 = deck::quickArclight(),
        Matches = 50,
        activeThreads := [],
        Anon = {  :- runStack(100, Matches, Deck1, Deck2) },
        _ = thread::start(Anon).

predicates
    runStack : (integer Runs, integer Matches, deck Deck1, deck Deck2).
clauses
    runStack(Runs, Matches, Deck1, Deck2) :-
        RunStack = varM::new([]),
        Total = varM_integer::new(0),
        Processed = varM_integer::new(0),
        NameVar = varM::new(""),
        foreach Num = std::fromTo(1, Runs) do
            if Num mod 2 = 1 then
                Player1 = player::new(Deck1, Deck1:getDecklist()),
                Player2 = player::new(Deck2, Deck2:getDecklist())
            else
                Player2 = player::new(Deck1, Deck1:getDecklist()),
                Player1 = player::new(Deck2, Deck2:getDecklist())
            end if,
            Anon =
                {  :-
                    Thread = thread::start({  :- runContest(Player1, Player2, Matches) }),
                    activeThreads := [Thread | activeThreads],
                    NameVar:value := Deck1:shortName
                },
            RunStack:value := [Anon | RunStack:value],
            Total:inc()
        end foreach,
        foreach Anon in RunStack:value do
            Anon(),
            if list::length(activeThreads) >= 15 then
                synchronize(NameVar:value, Processed, Total:value)
            end if
        end foreach,
        synchronize(NameVar:value, Processed, Total:value),
        printTop(2, 7),
        match::getManaHit(Hit, Tot),
        stream:writef("%d T4 songs of %d opportunities", Hit, Tot).

% This code is maintained automatically, do not update it manually.
%  17:03:45-13.8.2019
predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("goldfishing"),
        setDecoration(titlebar([closeButton, maximizeButton, minimizeButton])),
        setBorder(sizeBorder()),
        setState([wsf_ClipSiblings]),
        whenCreated({  :- projectToolbar::create(getVpiWindow()) }),
        addSizeListener({  :- vpiToolbar::resize(getVpiWindow()) }),
        setMdiProperty(mdiProperty),
        menuSet(resMenu(resourceIdentifiers::id_TaskMenu)),
        addShowListener(onShow),
        addSizeListener(onSizeChanged),
        addDestroyListener(onDestroy),
        addMenuItemListener(resourceIdentifiers::id_help_about, onHelpAbout),
        addMenuItemListener(resourceIdentifiers::id_file_new, onFileNew),
        addMenuItemListener(resourceIdentifiers::id_file_open, onFileOpen),
        addMenuItemListener(resourceIdentifiers::id_file_save, onFileSave),
        addMenuItemListener(resourceIdentifiers::id_edit_cut, onEditCut),
        addMenuItemListener(resourceIdentifiers::id_edit_undo, onEditUndo),
        addMenuItemListener(resourceIdentifiers::id_edit_redo, onEditRedo).
% end of automatic code

end implement taskWindow

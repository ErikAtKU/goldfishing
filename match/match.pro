% Copyright Prolog Development Center

implement match
    open core

class facts
    manaHit : tuple{integer, integer} := tuple(0, 0).

facts
    player1 : player.
    player2 : player.
    turn : integer.
    winner : player := erroneous.
    reason : string := erroneous.
    opponentLife : integer := erroneous.
    winningHand : card* := erroneous.
    winningPlay : card* := erroneous.
    winningGraveyard : card* := erroneous.
    winningExile : card* := erroneous.

clauses
    new(Player1, Player2) :-
        player1 := player::player(Player1),
        player2 := player::player(Player2),
        player1:opponent := player2,
        player2:opponent := player1,
        turn := 0,
        firstTurn().

predicates
    checkEnd : (predicate).
clauses
    checkEnd(Predicate) :-
        if isErroneous(winner) then
            if true = player1:hasLost then
                winner := player2,
                reason := player1:reason
            elseif true = player2:hasLost then
                winner := player1,
                reason := player2:reason
            elseif true = player1:hasWon then
                winner := player1,
                reason := player1:reason
            elseif true = player2:hasWon then
                winner := player2,
                reason := player2:reason
            else
                Predicate()
            end if,
            if player1:reason = "Mill" then
                markBoardstate(player2)
            elseif player2:reason = "Mill" then
                markBoardstate(player1)
            end if
        end if.

clauses
    getResults("", turn, "", "") :-
        isErroneous(winner),
        !.
    getResults(Player:name, Turn, reason, Boardstate) :-
        Player = winner,
        Turn = (turn + 1) div 2,
        if reason = "Mill" then
        elseif Song in Player:play and Song:name = deck::songOfCreationName and ! and Steamkin in Player:play
            and Steamkin:name = deck::runawaySteamkinName
        then
            reason := string::format("%d Steamkin Song", Player:spellsThisTurn)
        elseif Player:spellsThisTurn > 4 then
            reason := string::format("%d Spell combo", Player:spellsThisTurn)
        end if,
        Boardstate =
            string::format("%d life/%d spells: %p - %p - %p - %p", opponentLife, Player:spellsThisTurn, winningPlay, winningHand, winningGraveyard,
                winningExile).

clauses
    getManaHit(Hit, Total) :-
        tuple(Hit, Total) = manaHit.

predicates
    firstTurn : ().
clauses
    firstTurn() :-
        player1:firstDraw(),
        player2:firstDraw(),
        turn := 1,
        checkEnd(player1:untap),
        %checkEnd(player1:upkeep),
        checkEnd(player1:main),
        checkEnd(player1:combat),
        %checkEnd(player1:main),
        checkEnd(player1:endPhase),
        checkEnd(nextTurn).

predicates
    nextTurn : ().
clauses
    nextTurn() :-
        turn := turn + 1,
        if turn mod 2 = 0 then
            playerTurn(player2)
        else
            playerTurn(player1)
        end if.

predicates
    playerTurn : (player).
clauses
    playerTurn(Player) :-
        markBoardstate(Player),
        checkEnd(Player:untap),
        %checkEnd(Player:upkeep),
        markBoardstate(Player),
        checkEnd(Player:draw),
        markBoardstate(Player),
        checkEnd(Player:main),
        markBoardstate(Player),
        checkEnd(Player:combat),
        %checkEnd(Player:main),
        checkEnd(Player:endPhase),
        checkEnd(nextTurn).

predicates
    markBoardstate : (player).
clauses
    markBoardstate(Player) :-
        opponentLife := Player:opponent:life,
        winningHand := Player:hand,
        winningPlay := Player:play,
        winningGraveyard := Player:graveyard,
        winningExile := list::append(Player:exile, Player:companion).

end implement match

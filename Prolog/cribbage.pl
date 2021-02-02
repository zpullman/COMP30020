%%%%%%%%%%%%%%%%%%%%%%%   CRIBBAGE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*

Zac Pullman, 695145

COMP30020 Declarative Programming

This program calculates the value of a hand for the game of cribbage (hand_value),
it also attempts to choose the best hand at the start of a game of cribbage.


What is Cribbage?

The object of the game is to be the first player to reach 121 points. Game play
begins with the dealer dealing each player 6 cards (in a 2-player game) or 5
cards (in a 3 or 4 player game). In a 3 player game, the dealer also deals one
card to a separate hand called the crib or box. Each player then chooses 1 or 2
cards to discard, keeping 4 and putting the discarded cards in the crib or box,
which forms a second 4-card hand for the dealer.

Scoring a Hand;
1)  (15's) for each distinct combination that sums to 15 using the players hand and
    the Start card you score 2 points
2)  (Pairs) for every pair you score 2 points, three of kind scores 6 since there is
    essentially 3 pairs, similarly four of a kind scores 12
3)  (runs) 3 or more consecutive cards is a run, a run of 3 scores 3, 4 scores
    4 and 5 scores 5. so you cant count two seperate 3 runs when you have a run
    of 4. Also in cases when there is a pair that is included in the run you
    count that as 2 runs for 6 points i.e. 4,5,5,6,9 has two 4,5,6 runs.
4)  (Flushes) 1 point for 4 cards of the same suit, a bonus point if the Start
    card is also the same suit.
5)  ("One for his nob") 1 point if the hand contains the jack of the same suit
    as the Start card.

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%                SETTING UP THE CARDS

%% suits(-Suit) is deterministic.
% @param Suit is the Suit of the card.
%
% Defines Suits of cards; clubs, diamonds, hearts, and spades
%
suits(clubs).
suits(diamonds).
suits(hearts).
suits(spades).

%% ranks(-Rank) is deterministic.
% @param Rank is the rank of the card.
%
% Defines Ranks of cards; ace, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king
%
ranks(ace).
ranks(2).
ranks(3).
ranks(4).
ranks(5).
ranks(6).
ranks(7).
ranks(8).
ranks(9).
ranks(10).
ranks(jack).
ranks(queen).
ranks(king).

%% successor(+Rank, +Rank) is deterministic
%% successor(-Rank, -Rank) is non deterministic
% @param Rank is the rank of the card
%
% Defines the ordering of cards by Rank, high card is the first input
%
succeeds(king,queen).
succeeds(queen,jack).
succeeds(jack,10).
succeeds(10,9).
succeeds(9,8).
succeeds(8,7).
succeeds(7,6).
succeeds(6,5).
succeeds(5,4).
succeeds(4,3).
succeeds(3,2).
succeeds(2,ace).

%% card(+Rank, +Suit) is deterministic.
%% card(-Rank, -Suit) is non deterministic.
% @param Rank is the rank of the card.
% @param Suit is the suit of the card.
%
% Defines card using Rank and Suit
%
card(Rank, Suit):-
    ranks(Rank),suits(Suit).

%% deck(-Deck) is deterministic
% generates a deck of cards; card(Rank,Suit)
% @Param Deck is the generated deck of cards
%
% generates a standard 52 card deck
%
deck(Deck):-
    findall(card(Rank, Suit),card(Rank, Suit),Deck).

%% SORTING HANDS, GETTING FACE VALUES AND ADDING CARD TO HAND

% compare_values(?Order, +Card1, +Card2) non-deterministic
% @Param Order is the Order between two terms in the standard
%
% order of terms. Order is one of <, > or =, with the obvious meaning.
% uses nth0 to give ordering of the cards and compare the values.
% added X == Y clause to stop deletion of duplicates when using predsort
%
% https://stackoverflow.com/questions/11852226/sort-a-list-of-cards-prolog
%
compare_values(D, card(A,_), card(B,_)) :-
    nth0(X, [ace, 2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king], A),
    nth0(Y, [ace, 2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king], B),
    (X == Y ;
    compare(D, X, Y)).

%%sort_cards(+Hand,-Sorted) is deterministic
% @Param Hand is a list of cards; card(Rank,Suit)
% @Param Sorted is the sorted list of cards
%
% takes Hand and sorts using predsort with the above compare values function.
% required the cut to stop other possible solutions that were produced in
% compare_values (from OR statement at the end of the function). This cut takes
% advantage of the top to bottom computation of prolog to ensure we keep all the
% cards that were given in the hand.
%
sort_cards(Hand, Sorted) :-
    predsort(compare_values, Hand, Sorted),!.

%%add_card_to_hand(+Hand, +Card, -Full_Hand)
% @Param Hand is a list of Cards
% @Param Card is a Card element; card(Rank,Suit)
% @Param Full_Hand is the outputted hand once the card is added
%
% appends a single card to a list of cards i.e. the hand
%
add_card_to_hand(Hand,Card,Full_Hand):-
    Temp = [Card],
    append(Hand,Temp,Full_Hand).

%% face_value(+Card, -Value) is deterministic
% @Param Card is the card who's face value you want; card(Rank, Suit)
% @Param Value is the face value of the given card.
%
% given a card as input outputs the face value of that card.
%
face_value(card(R,_), Value):-
    R = ace -> Value is 1;
    R = king -> Value is 10;
    R = queen -> Value is 10;
    R = jack -> Value is 10;
    otherwise -> Value is R.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%                    CALCULATING INDIVIDUAL COMPONENTS

%% count_pairs(+Hand,-Value)
% @Param Hand is a list of 5 cards and is sorted; card(Rank,Suit)
% @Param Value is the outputted value
%
% Given an inputed 5 card hand, calculated the points for that pair combination.
%
% Four of a kind
count_pairs([card(B,_),card(A,_),card(A,_),card(A,_),card(C,_)],12):-
    B = A ; C = A.

% Three of a kind
count_pairs([card(A,_),card(B,_),card(C,_),card(D,_), card(E,_)],6):-
    (A = B, B = C); (B = C, C = D); (C = D, D = E).

% two pair
count_pairs([card(A,_),card(A,_),card(B,_),card(B,_),card(_,_)],4).
count_pairs([card(A,_),card(A,_),card(_,_),card(B,_),card(B,_)],4).
count_pairs([card(_,_),card(A,_),card(A,_),card(B,_),card(B,_)],4).

% Pair
count_pairs([card(A,_),card(B,_),card(C,_),card(D,_),card(E,_)],2):-
    A = B; B = C; C = D; D = E.

% no pairs
count_pairs([card(A,_),card(B,_),card(C,_),card(D,_),card(E,_)],0):-
    A\=B, B\=C, C\=D, D\=E.

%% count_runs(+Hand,-Value)
% @Param Hand is a list of 5 cards and is sorted; card(Rank,Suit)
% @Param Value is the outputted value
%
% Calculates all the ways that runs can be scored
%
% calculates score for runs of 5, runs of 4 with and without pairs
count_runs([card(A,_),card(B,_),card(C,_),card(D,_),card(E,_)],Val):-
    (succeeds(E,D),succeeds(D,C),succeeds(C,B),succeeds(B,A), Val is 5)
    ; (succeeds(E,D), succeeds(D,C), succeeds(C,B), A \= B, Val is 4)
    ; (succeeds(D,C), succeeds(C,B), succeeds(B,A), E \= D, Val is 4)
    ; (succeeds(E,D), succeeds(D,C), succeeds(C,B), A = B, Val is 8)
    ; (succeeds(D,C), succeeds(C,B), succeeds(B,A), E = D, Val is 8)
    ; (succeeds(E,D), succeeds(D,B), succeeds(B,A), C = D, Val is 8)
    ; (succeeds(E,D), succeeds(D,C), succeeds(C,A), B = C, Val is 8).

% Calculates score for the combinations of runs of 3 with two pairs
count_runs([card(A,_),card(A,_),card(B,_),card(C,_),card(C,_)],Val):-
    succeeds(C,B), succeeds(B,A), Val is 12.
count_runs([card(A,_),card(A,_),card(B,_),card(B,_),card(C,_)],Val):-
    succeeds(C,B), succeeds(B,A), Val is 12.
count_runs([card(A,_),card(B,_),card(B,_),card(C,_),card(C,_)],Val):-
    succeeds(C,B), succeeds(B,A), Val is 12.

% Calculates score for the combinations of runs of 3 with one pair
count_runs([card(P1,_),card(A,_),card(B,_),card(C,_),card(P2,_)],Val):-
    succeeds(C,B), succeeds(B,A),(P1 = A ; P2 = C), Val is 6.
count_runs([card(A,_),card(B,_),card(P,_),card(C,_),card(D,_)],Val):-
    ((succeeds(D,C), succeeds(C,B), P = B)
    ;(succeeds(C,B),succeeds(B,A), P = C))
    ,Val is 6.

% calculates 3 card runs no pairs and give no run case
count_runs([card(A,_),card(B,_),card(C,_),card(D,_),card(E,_)],Val):-
    (((succeeds(E,D), succeeds(D,C))
    ; (succeeds(D,C), succeeds(C,B))
    ; (succeeds(C,B), succeeds(B,A))),
    Val is 3)
    ; Val is 0.



%% one_for_his_nob(+Hand, +Startcard, -Value)
% @Param Hand is a list of 4 cards (no ordering); card(Rank,Suit)
% @Param Startcard is the startcard
% @Param Value is the outputted value
%
% Checks for the rule of "one for his nob" and checks cases where it's not true
%
one_for_his_nob([card(A,S1),card(B,S2),card(C,S3),card(D,S4)], card(_,S),Val):-
    ((A = jack, S1 = S) ; (B = jack, S2 = S); (C = jack, S3 = S)
    ; (D = jack, S4 = S)), Val is 1.
one_for_his_nob([card(A,S1),card(B,S2),card(C,S3),card(D,S4)], card(_,S),Val):-
    ((A = jack, S1 \= S) ; (B = jack, S2 \= S); (C = jack, S3 \= S)
    ; (D = jack, S4 \= S)), Val is 0.
one_for_his_nob([card(A,_),card(B,_),card(C,_),card(D,_)], card(_,_),Val):-
    A \= jack, B \= jack, C \= jack, D \= jack, Val is 0.

%%flushes(+Hand, +StartCard, Value)
% @Param Hand is a list of 4 cards (no ordering); card(Rank,Suit)
% @Param Startcard is the startcard
% @Param Value is the outputted value
%
% Checks if the suit of the cards in the hand match with start card or
% if they match just within the hand and set
%
flushes([card(_,S1),card(_,S2),card(_,S3),card(_,S4)], card(_,S), Val):-
    (S1 = S2, S2 = S3, S3 = S4, S4 = S, Val is 5 )
    ;
    (S1 = S2, S2 = S3, S3 = S4, S4 \= S, Val is 4 )
    ;
    Val is 0.

%% fifteens(+Hand, -Value)
% @Param Hand is a list of 5 cards (no ordering); card(Rank,Suit)
% @Param Value is the outputted value
% fifteens splits the hand into all of its permutations using (sublist)
% and setof makes sure these are unique. then it's fed into fifteens/3 to do
% the actual calculation.
fifteens(Hand, Value):-
    setof(X,sublist(X,Hand),Z), % Z is a list of all the sublists of the hand i.e.
    fifteens(Z,0,Value).        % list of lists

% fifteens(+ListofHands, +Accumulator, -Value)
% @Param ListofHands is all sublists of the hand given in fifteens/2
% @Param Accumulator a placeholder to sum the score
% @Param Value is the score you get for all combinations of 15 possible for the
% given hand.
%
% using the List of sublists of the hand, check if the list sums to 15, then we
% add 2 to the accumulator and continue with the next possible combination of
% cards. Otherwise just continue without adding.
%
fifteens([],A,V):-
    V is A.                   % A is an Accumulator
fifteens([Hand|OtherHands], A,Value):-
    sum_hand(Hand, Sum),
    (Sum =:= 15 ->
        A1 is A + 2,
        fifteens(OtherHands,A1,Value)
    ;   fifteens(OtherHands,A,Value)
    ).

%%sublist(-Sublist, +List)
% @Param List consisting of any type
% @Param Sublist is the sublists with the same type as List
%
% generates all possible sublists of a given list
%
sublist([], _).
sublist([X|Xs], [X|Ys]):-
    sublist(Xs, Ys).
sublist(Xs, [_|Ys]):-
    sublist(Xs, Ys).

%sum_hand(+Hand, -Sum)
% @Param Hand list of cards (card(Rank,Suit) can be any length
% @Param Sum the sum of the face values of all the cards in the hand.
%
% Sums up the face value of the cards in any given hand
%
sum_hand(Hand, Sum) :-
	sum_hand(Hand, Sum, 0).
sum_hand([Card|Hand], Sum, A) :-
	face_value(Card, Q),
	A1 is A + Q,
	sum_hand(Hand, Sum, A1).
sum_hand([], Sum, V) :-
	Sum is V.

%% hand_value(+Hand,+StartCard,-Value)
% @Param Hand is a list of cards of length 4; card(Rank,Suit)
% @Param StartCard is a card
% @Param Value is the outputed value of a given hand i.e. the score for that hand
%
% Hand value checks all the ways you can score points in scribbage and sums them
% to give the total value for that hand and startcard combination.
%
hand_value(Hand, StartCard, Value):-
    one_for_his_nob(Hand, StartCard, V1),
    flushes(Hand,StartCard,V2),
    add_card_to_hand(Hand, StartCard, Full_Hand),
    sort_cards(Full_Hand, Sorted),
    count_pairs(Sorted,V3),
    fifteens(Sorted,V4),
    count_runs(Sorted, V5),
    Value is V1+V2+V3+V4+V5,!.

%% select_hand(+Cards, -Hand, -Cribcards)
% @Param Cards is a list of cards of length 5 to 6; card(Rank,Suit)
% @Param Hand is the best hand of 4 cards, given the list Cards
% @Param Cribcards are the cards not used in the Hand
%
% creates a list of all the possible hands and make a list of all the start
% cards possibly left in the deck. use find_best_hand to calculate the expected
% values of all the hands and return the best hand.
%

select_hand(Cards, Hand, Cribcards):-
    setof(X,(sublist(X,Cards), length(X,4)), PossibleHands),
    deck(Deck),
    subtract(Deck, Cards, StartCards),    %Startcards is a list of potential Startcards
    find_best_hand(PossibleHands, StartCards, Hand),
    subtract(Cards,Hand,Cribcards).



%% find_best_hand(+PossibleHands, +StartCards, -BestHand)
% @Param PossibleHands is a list of lists of cards of length 4
% @Param StartCards is a list of all possible start cards
% @Param BestHand is a list of 4 cards that gives the highest expected value
% to score points
%
% calculate the expected value of a hand and compare it to the current best value
% if that value is higher, then we set that to be the new best value and
% update the best hand too.
%
% had to use a cut to stop infinite looping
%
find_best_hand(PossibleHands, StartCards, BestHand):-
    find_best_hand(PossibleHands, StartCards, 0, [],BestHand),!.

find_best_hand([], _, _, CurrentBest, BestHand):-
    BestHand = CurrentBest.
find_best_hand([H|T], StartCards, Value, CurrentBest, BestHand):-
    expected_hand_value(H,StartCards,V),
    (V > Value ->
        find_best_hand(T, StartCards, V, H, BestHand)
    ;   find_best_hand(T, StartCards, Value, CurrentBest, BestHand)
    ).

%% expected_hand_value(+Hand, +StartCards, -Value
% @Param Hand is a list of cards of length 4
% @Param StartCards is a list of all possible start cards
%
% Given a hand and a list of possible startcards calculate the sum of the
% hand with respect to each start card.
%
expected_hand_value(Hand, StartCards, Value):-
    expected_hand_value(Hand, StartCards, 0,Value).

expected_hand_value(_, [], A,Value):-
    Value is A.
expected_hand_value(Hand, [H|T], A,Value):-
    hand_value(Hand, H, V),
    A1 is A + V,
    expected_hand_value(Hand, T, A1, Value).

/*
%%%%%%%%%%%%%%%%%%%%%  TEST CASES AND SOLUTIONS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hand_value([card(7,clubs),card(queen,hearts),card(2,clubs),card(jack,clubs)], card(9,hearts), Value).
Value = 0.

hand_value([card(ace,spades),card(3,hearts),card(king,hearts),card(7,hearts)], card(king,spades), Value).
Value = 2.

hand_value([card(ace,spades),card(3,hearts),card(king,hearts),card(7,hearts)], card(2,diamonds), Value).
Value = 5.

hand_value([card(6,clubs),card(7,clubs),card(8,clubs),card(9,clubs)], card(8,spades), Value).
Value = 20.

hand_value([card(7,hearts),card(9,spades),card(8,clubs),card(7,clubs)], card(8,hearts), Value).
Value = 24.

hand_value([card(5,hearts),card(5,spades),card(5,clubs),card(jack,diamonds)], card(5,diamonds), Value).
Value = 29.

*/

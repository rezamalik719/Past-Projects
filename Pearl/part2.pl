:- [hogwarts].
:- [part2Helper].

/*
 * From the list of constraints by house, find the price and volume constraint
 * for 'House'
 */
find_house([], House, []).
find_house([C | T], House, R) :-
    nth0(0, C, Current_House),
    (Current_House = House ->
        append(C, [], R);
        find_house(T, House, R)
    ).

% NLP PARSER
%
/*
 * Base case when no lines are left to parse; begins optimizing
 * for the constraints of each house
*/ 
read_loop([], GStats, SStats, HStats, RStats, Constraints) :-
    % Get Constraint List for each house
    find_house(Constraints, gryffindor, GC),
    find_house(Constraints, slytherin, SC),
    find_house(Constraints, ravenclaw, RC),
    find_house(Constraints, hufflepuff, HC),
    % Only optimize for those houses for whom constraints were given
    % Can check this by seeing the length of each constraint list
    length(GC, GCL),
    length(SC, SCL),
    length(RC, RCL),
    length(HC, HCL),

    % Optimize for gryffindor
    (GCL > 0 ->
        write("gryffindor\n"),
        optimize(GStats, [], GC); true),
    % Optimize for slytherin
    (SCL > 0 ->
        write("slytherin\n"),
        optimize(SStats, [], SC); true),
    % Optimize for ravenclaw
    (RCL > 0 ->
        write("ravenclaw\n"),
        optimize(RStats, [], RC); true),
    % Optimize for hufflepuff
    (HCL > 0 ->
        write("hufflepuff\n"),
        optimize(HStats, [], HC); true).

/* 
 * Recursive parser evaluating one line at a time
 * NOTE: A "naive extraction method" is used here by
 * merely parsing terms by their position in the
 * two different sentences. No DCG is used.
 */
read_loop([Line|Tail], GStats, SStats, HStats, RStats, Constraints) :-
    % Get the length of the current line
    length(Line, Length),
    % If the line has 15 items, we know we are parsing an item
	(Length =:= 15 ->
        % Person's name
		nth0(0, Line, Person),
        % Given item's name / label:w
		nth0(3, Line, Item),
        % Dollar value
		nth0(6, Line, V1),
        % Volume in cubic feet
		nth0(11, Line, V2),
        % Get the house of the individual from hogwarts.pl
        houseOf(House, Person),
        % If the house is gryffindor, add the person's name, the item
        % the dollar value, and volume to a list of gryffindor's items
        % Recurse afterward, leaving the constraints and the list of 
        % other houses unchanged
        (House = gryffindor ->
            append(GStats, [[Person, Item, V1, V2]], NGStats),
            append(SStats, [], NSStats),
            append(HStats, [], NHStats),
            append(RStats, [], NRStats),
            append(Constraints, [], NConstraints),
            read_loop(Tail, NGStats, NSStats, NHStats, NRStats, NConstraints)
        ; true),
        % Add to slytherin's list 
        (House = slytherin ->
            append(SStats, [[Person, Item, V1, V2]], NSStats),
            append(GStats, [], NGStats),
            append(HStats, [], NHStats),
            append(RStats, [], NRStats),
            append(Constraints, [], NConstraints),
            read_loop(Tail, NGStats, NSStats, NHStats, NRStats, NConstraints)
        ; true),
        % Add to hufflepuff's list 
        (House = hufflepuff ->
            append(HStats, [[Person, Item, V1, V2]], NHStats),
            append(SStats, [], NSStats),
            append(GStats, [], NGStats),
            append(RStats, [], NRStats),
            append(Constraints, [], NConstraints),
            read_loop(Tail, NGStats, NSStats, NHStats, NRStats, NConstraints)
        ; true),
        % Add to ravenclaw's list 
        (House = ravenclaw ->
            append(RStats, [[Person, Item, V1, V2]], NRStats),
            append(SStats, [], NSStats),
            append(HStats, [], NHStats),
            append(GStats, [], NGStats),
            append(Constraints, [], NConstraints),
            read_loop(Tail, NGStats, NSStats, NHStats, NRStats, NConstraints)
        ; true); true
    ),
    % If the # elements in the line is 18, we are parsing a constraint
    (Length =:= 18 ->
        % Most elements we need to parse are in a fixed position 
		nth0(0, Line, House),
        nth0(4, Line, A1),
        nth0(5, Line, Comp1),
        nth0(7, Line, V1),
        % If price is listed first, the last three pieces of info
        % are one index closer to the beginning
        (A1 = price ->
            nth0(11, Line, A2),
            nth0(12, Line, Comp2),
            nth0(14, Line, V2),
            append(Constraints, [[House, Comp1, V1, Comp2, V2]], NConstraints),
            append(SStats, [], NSStats),
            append(HStats, [], NHStats),
            append(GStats, [], NGStats),
            append(RStats, [], NRStats),
	        read_loop(Tail, NGStats, NSStats, NHStats, NRStats, NConstraints);
            % If volume is listed first, the remaining elements are offset by 1
            % due to "cubic feet" having one more elem than "price"
            nth0(12, Line, A2),
            nth0(13, Line, Comp2),
            nth0(15, Line, V2),
            append(Constraints, [[House, Comp2, V2, Comp1, V1]], NConstraints),
            append(SStats, [], NSStats),
            append(HStats, [], NHStats),
            append(GStats, [], NGStats),
            append(RStats, [], NRStats),
	        read_loop(Tail, NGStats, NSStats, NHStats, NRStats, NConstraints)
	    ); true
    ).
    

% CONSTRAINT SATISFACTION 

% Sum the combined cost of the items in the given list defined by [H | T]
sum_price([], 0).
sum_price([H | T], Total) :-
    % Use Subtotal as goal
    sum_price(T, Subtotal),
    nth0(2, H, Price),
    % Convert Price to an integer (it was parsed as an atom)
    atom_number(Price, Price_Val),
    % Add current item's price to subtotal
    Total is Price_Val + Subtotal.

% Sum the combined volume of the items in the given list defined by [H | T]
sum_volume([], 0).
sum_volume([H | T], Total) :-
    % Use Subtotal as goal
    sum_volume(T, Subtotal),
    nth0(3, H, Volume),
    % Convert Volume to an integer (it was parsed as an atom)
    atom_number(Volume, Volume_Val),
    % Add current item's volume to subtotal
    Total is Volume_Val + Subtotal.


% Form a list of just the names / labels of the items from the list of all items
item_list([], []).
item_list([H |T], R) :-
    item_list(T, R1),
    nth0(1, H, Item),
    append(R1, [Item], R).


% Check if the list of items L satisfies the constraint given by Constraint
constraint_check(L, Constraint) :-
    % Extract info from Constraint
    nth0(1, Constraint, PriceComp),
    nth0(2, Constraint, PriceAtom),
    atom_number(PriceAtom, PriceConst), 
    nth0(3, Constraint, VolumeComp),
    nth0(4, Constraint, VolumeAtom),
    atom_number(VolumeAtom, VolumeConst), 
    
    % Sum the prices and volumes of each item in L
    sum_price(L, Total_Cost), 
    sum_volume(L, Total_Volume),
    % Get the names of each item
    item_list(L, Items),
    
    % Comparisons to verify price and volume are appropriately less than 
    % or greater than as the constraint specifies
    % Only write to the console if the constraints are satisfied
    (PriceComp = less -> 
        (Total_Cost < PriceConst ->
            (VolumeComp = less ->
                (Total_Volume < VolumeConst ->
                    write(" "),
                    write([Total_Cost, Total_Volume]),
                    write(":"), 
                    write(Items),
                    write("\n")
                ; true
                ); Total_Volume > VolumeConst ->
                    write(" "),
                    write([Total_Cost, Total_Volume]),
                    write(":"), 
                    write(Items),
                    write("\n"); true
             ); true
        );
     Total_Cost > PriceConst ->
            (VolumeComp = less ->
                (Total_Volume < VolumeConst ->
                    write(" "),
                    write([Total_Cost, Total_Volume]),
                    write(":"), 
                    write(Items),
                    write("\n")
                ; true
                ); Total_Volume > VolumeConst ->
                    write(" "),
                    write([Total_Cost, Total_Volume]),
                    write(":"), 
                    write(Items),
                    write("\n"); true
             )
     ; true).

% Optimize's base case; begin constraint checking when there are no more
% combinations to generate
optimize([H], R, Constraint) :-
    append(R, [H], R1),
    append(R, [], R2),
    
    % Check if R1 and R2 satisfy the constraints
    constraint_check(R1, Constraint),
    % Only check R2 if it is non-empty
    length(R2, Len2),
    (Len2 > 0 -> constraint_check(R2, Constraint); true).

% MAIN DRIVER FOR CONSTRAINT PROBLEM
% Optimize for the constraint by finding all possible combinations
% of items that satisfy it; this is achieved by finding all combinations
% of the given list defined by [H | T]
% For each item, we consider the possibility of taking or not taking it
optimize([H | T], R, Constraint) :-
    % Take H or don't take H
    append(R, [H], R1),
    append(R, [], R2),
    optimize(T, R1, Constraint),
    optimize(T, R2, Constraint).
    

    
% Main 
main :-
    current_prolog_flag(argv, [DataFile|_]),
    open(DataFile, read, Stream),
    read_file(Stream,Lines), %Lines contain all the information within line split by spaces, comma and period.
    read_loop(Lines, [], [], [], [], []),
    close(Stream).
	

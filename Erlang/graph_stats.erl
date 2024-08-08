-module(graph_stats).
-export([start/3, part_a/0, write_results_to_file/3, cell_part_a/4, spawn_actors_part_a/2, spawn_actors_part_b/2, store_actor_as_key/3, cell_part_b/2,combine_node_lists/2, get_degrees/4, part_b_util/6, inf_node_cell/7, global_inf_nodes_cell/4]).

% file parsing from https://stackoverflow.com/questions/2475270/how-to-read-the-contents-of-a-file-in-erlang
readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
      after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.

groups_of_four(List, Iter) ->
    if 
        Iter > length(List) -> 
            [];
        true -> 
            [lists:sublist(List, Iter, 4)] ++ groups_of_four(List, Iter+4)
    end.

to_int([]) -> [];
to_int([H|T]) ->
    {Int, _} = string:to_integer(H),
    [Int] ++ to_int(T).

make_empty_adj_list([], []) -> [];
make_empty_adj_list([Node1|T1], [Color1| T2]) ->
    [{Node1, [Color1]}] ++ make_empty_adj_list(T1, T2).

add_edges_to_adj_list([], Adj_list) -> Adj_list;
add_edges_to_adj_list([E| T], Adj_list) ->
    Edge = to_int(string:tokens(E, ",")),
    % If a node in this edge is in a different partition, only add that node to one of the lists
    Adj_list1 = dict:append(lists:nth(1, Edge), lists:nth(2, Edge), Adj_list),
    Adj_list2 = dict:append(lists:nth(2, Edge), lists:nth(1, Edge), Adj_list1),
    Adj_list3 = dict:append(lists:nth(2, Edge), lists:nth(1, Edge), Adj_list),
    case lists:member(lists:nth(1, Edge), dict:fetch_keys(Adj_list)) of
        false ->
                add_edges_to_adj_list(T, Adj_list3);
        true ->
            case lists:member(lists:nth(2, Edge), dict:fetch_keys(Adj_list)) of
                true -> 
                    add_edges_to_adj_list(T, Adj_list2);
                false ->
                    add_edges_to_adj_list(T, Adj_list1)
            end
    end.


parse(Partition_info) ->
    {Partition_no, _} = string:to_integer(lists:nth(2, string:tokens(lists:nth(1, Partition_info), " "))),
    Nodes = to_int(string:tokens(lists:nth(2, Partition_info), ",")),
    Colors = string:tokens(lists:nth(3, Partition_info), ","),
    Adj_list = dict:from_list(make_empty_adj_list(Nodes, Colors)),
    Edges = string:tokens(lists:nth(4, Partition_info), " "),
    {Partition_no, add_edges_to_adj_list(Edges, Adj_list)}.

parse_each(Partition_info_list, Iter) ->
    if 
        Iter > length(Partition_info_list) ->
        [];
    true ->
        [parse(lists:nth(Iter, Partition_info_list))] ++ parse_each(Partition_info_list,Iter+1)
    end.

add_entries(OldDict, NewDict, []) -> OldDict;
add_entries(OldDict, NewDict, [NewDictKey1 | T]) ->
        % add each color to new_dict
        {_, Values} = dict:find(NewDictKey1, NewDict),
        [A, B] = Values,
        % if key present, add to existing values of that color 
        % if not, add color to content
        FinalDict = dict:update(NewDictKey1, fun(Old) -> [A+lists:nth(1, Old), B+lists:nth(2, Old)] end, [A, B], OldDict),
        add_entries(FinalDict, NewDict, T).

% Cell to add up results of all partitions
% Content takes the form of a dictionary, where 
% keys are colors and values are lists of two numbers
cell_part_a(Content, Partitions_computed, Partitions_to_compute, Output_file_path) -> 
        receive
          {set, NewContent} ->
            % Add results of each color from partition to original content
            FinalContent = add_entries(Content, NewContent, dict:fetch_keys(NewContent)),
            % write results to file if partitions_computed == Partitions_to_compute
            if 
                Partitions_computed+1 == Partitions_to_compute ->
                    write_results_to_file(FinalContent, dict:fetch_keys(FinalContent), Output_file_path);
                true ->
                    ok
            end,
            cell_part_a(FinalContent, Partitions_computed+1, Partitions_to_compute, Output_file_path);
          {get, Customer}   -> Customer ! Content, 
                               cell_part_a(Content, Partitions_computed, Partitions_to_compute, Output_file_path)
        end.

% returns list of actors
spawn_actors_part_a([], Stats_cell) -> ok;
spawn_actors_part_a([P | T], Stats_cell) ->
    {Partition_no, Partition} = P,
    A = spawn(graph_stats, part_a, []),
    A ! {Partition, Stats_cell},
    spawn_actors_part_a(T, Stats_cell).

part_a_util(Partition, [], Stats_Dict) -> Stats_Dict;
part_a_util(Partition, [Key1 | T], Stats_Dict) ->
    Node_data = dict:fetch(Key1, Partition),
    Node_color = lists:nth(1, Node_data),
    Node_degree = length(Node_data) - 1,
    case lists:member(Node_color, dict:fetch_keys(Stats_Dict)) of
        true ->
            Stats_Dict_New = dict:update(Node_color, fun ([Count, Degree]) -> [Count+1, Degree + Node_degree] end, Stats_Dict),
            part_a_util(Partition, T, Stats_Dict_New);
        false ->
            Stats_Dict_New = dict:append_list(Node_color, [1, Node_degree], Stats_Dict),
            part_a_util(Partition, T, Stats_Dict_New)
    end.

part_a() ->
    receive {Partition, Stats_cell} ->
        Stats_dict = part_a_util(Partition, dict:fetch_keys(Partition), dict:from_list([])),
        Stats_cell ! {set, Stats_dict}
    end,
    part_a().


cell_part_b(Partition_no, Partition) ->
    receive {get, Node, Customer} ->
        case lists:member(Node, dict:fetch_keys(Partition)) of
            false -> Customer ! {set, Node, 0};
            true ->
                {_, Entries} = dict:find(Node, Partition),
                Customer ! {set, Node, length(Entries) - 1}
        end;
        {go, Nodes_to_actors, No_actors, Output_file_path, Global_customer} ->
            part_b_util(Partition_no, Partition, Nodes_to_actors, No_actors, Output_file_path, Global_customer)
    end,
    cell_part_b(Partition_no, Partition).

store_actor_as_key([], Nodes_to_actors, Actor) -> Nodes_to_actors;
store_actor_as_key([Node1 | T], Nodes_to_actors, Actor) ->
    New_Nodes_to_actors = dict:store(Node1, Actor, Nodes_to_actors),
    store_actor_as_key(T, New_Nodes_to_actors,Actor).

spawn_actors_part_b([], Nodes_to_actors) -> Nodes_to_actors;
spawn_actors_part_b([P | T], Nodes_to_actors) ->
    {Partition_no, Partition} = P,
    % Spawn actor using cell_part_b
    A = spawn(graph_stats, cell_part_b, [Partition_no, Partition]),
    % Add each node in partition as a key to Nodes_to_actors with the spawned actor as a value
    New_Nodes_to_actors = store_actor_as_key(dict:fetch_keys(Partition), Nodes_to_actors, A),
    % Return Nodes_to_actors
    spawn_actors_part_b(T, New_Nodes_to_actors).

% Computes influential node in partition
% Messages other actors when necessary
combine_node_lists(Partition, []) -> [];
combine_node_lists(Partition, [Node1 | T]) ->
    {_, Entries} = dict:find(Node1, Partition),
    Adjacent_nodes = lists:sublist(Entries, 2, length(Entries) - 1),
    Adjacent_nodes ++ combine_node_lists(Partition, T).

list_to_string([Node1 | T]) ->
    if length(T) > 0 ->
            string:concat(io_lib:format("~p,", [Node1]), list_to_string(T));
        true ->
            io_lib:format("~p", [Node1])
    end.

global_inf_nodes_cell(All_inf_nodes, Partitions_computed, Partitions_to_compute, Output_file_path) ->
    receive {set, Some_inf_nodes} ->
                if Partitions_computed + 1 == Partitions_to_compute ->
                    file:write_file(Output_file_path, io_lib:format("G: ~s", [list_to_string(lists:sort(sets:to_list(sets:from_list(All_inf_nodes ++ Some_inf_nodes))))]), [append]),
                    global_inf_nodes_cell(sets:to_list(sets:from_list(All_inf_nodes ++ Some_inf_nodes)), Partitions_computed+1, Partitions_to_compute, Output_file_path);
                true ->
                    global_inf_nodes_cell(sets:to_list(sets:from_list(All_inf_nodes ++ Some_inf_nodes)), Partitions_computed+1, Partitions_to_compute, Output_file_path)
                end
    end.


inf_node_cell(Partition_no, Nodes_checked, Nodes_to_check, Current_max_degree, Corresponding_nodes, Customer, Output_file_path) -> 
    receive
        {set, Node, NewDegree} -> 
            if Nodes_checked + 1 == Nodes_to_check ->
                if NewDegree > Current_max_degree ->
                    New_max_degree = NewDegree,
                    New_corresponding_nodes = [Node],
                    S = list_to_string(lists:sort(New_corresponding_nodes)),
                    file:write_file(Output_file_path, io_lib:format("partition ~p: ~s\n", [Partition_no, S]), [append]),
                    Customer ! {set, New_corresponding_nodes},
                    inf_node_cell(Partition_no, Nodes_checked+1, Nodes_to_check, New_max_degree, New_corresponding_nodes, Customer, Output_file_path);
                NewDegree == Current_max_degree ->
                    New_max_degree = Current_max_degree,
                    New_corresponding_nodes = Corresponding_nodes ++ [Node],
                    S = list_to_string(lists:sort(New_corresponding_nodes)),
                    file:write_file(Output_file_path, io_lib:format("partition ~p: ~s\n", [Partition_no, S]), [append]),
                    Customer ! {set, New_corresponding_nodes},
                    inf_node_cell(Partition_no, Nodes_checked+1, Nodes_to_check, New_max_degree, New_corresponding_nodes, Customer, Output_file_path);
                true ->
                    New_max_degree = Current_max_degree,
                    New_corresponding_nodes = Corresponding_nodes,
                    S = list_to_string(lists:sort(New_corresponding_nodes)),
                    file:write_file(Output_file_path, io_lib:format("partition ~p: ~s\n", [Partition_no, S]), [append]),
                    Customer ! {New_corresponding_nodes},
                    inf_node_cell(Partition_no, Nodes_checked+1, Nodes_to_check, New_max_degree, New_corresponding_nodes, Customer, Output_file_path)
                end,
                ok;
                true ->
                    if NewDegree > Current_max_degree ->
                        inf_node_cell(Partition_no, Nodes_checked+1, Nodes_to_check, NewDegree, [Node], Customer, Output_file_path);
                    NewDegree == Current_max_degree ->
                        inf_node_cell(Partition_no, Nodes_checked+1, Nodes_to_check, NewDegree, Corresponding_nodes ++ [Node], Customer, Output_file_path);
                    true ->
                        inf_node_cell(Partition_no, Nodes_checked+1, Nodes_to_check, Current_max_degree, Corresponding_nodes, Customer, Output_file_path)
                    end
            end
    end.


get_degrees(Partition, [], Nodes_to_actors, Inf_node_cell) -> [];
get_degrees(Partition, [Node1 | T], Nodes_to_actors, Inf_node_cell) ->
    case lists:member(Node1, dict:fetch_keys(Partition)) of
        true ->
            {_, Entries} = dict:find(Node1, Partition),
            Inf_node_cell ! {set, Node1, length(Entries) - 1},
            get_degrees(Partition, T, Nodes_to_actors, Inf_node_cell);
        false ->
            % message other actor
            {_, Foreign_partition} = dict:find(Node1, Nodes_to_actors),
            Foreign_partition ! {get, Node1, Inf_node_cell},
            get_degrees(Partition, T, Nodes_to_actors, Inf_node_cell)
    end.

part_b_util(Partition_no, Partition, Nodes_to_actors, No_actors, Output_file_path, Global_customer) ->
    % Get all nodes in the partition and connected to the partition
    All_nodes = sets:to_list(sets:from_list(combine_node_lists(Partition, dict:fetch_keys(Partition)))),
    % Get degree of all nodes
    Inf_node_cell = spawn(graph_stats, inf_node_cell, [Partition_no, 0, length(All_nodes), 0, [], Global_customer, Output_file_path]),
    get_degrees(Partition, All_nodes, Nodes_to_actors, Inf_node_cell).
    

write_results_to_file (Stats_dict, [Key1 | T], Output_file_path) ->
    Data = dict:fetch(Key1, Stats_dict),
    Total_keys = length(dict:fetch_keys(Stats_dict)),
    if 
        length(T) + 1 == Total_keys ->
            file:write_file(Output_file_path, io_lib:format("~s, ~p, ~p\n", [Key1, lists:nth(1, Data), lists:nth(2, Data)])),
            write_results_to_file(Stats_dict, T, Output_file_path);
        length(T) > 0 ->
            file:write_file(Output_file_path, io_lib:format("~s, ~p, ~p\n", [Key1, lists:nth(1, Data), lists:nth(2, Data)]), [append]),
            write_results_to_file(Stats_dict, T, Output_file_path);
        true -> 
            file:write_file(Output_file_path, io_lib:format("~s, ~p, ~p", [Key1, lists:nth(1, Data), lists:nth(2, Data)]), [append]) 
    end.

second_elem([]) -> [];
second_elem([{Key, Value} | T]) ->
    [Value] ++ second_elem(T).

go_all_actors_part_b([], Nodes_to_actors, No_actors, Output_file_path, Global_customer) -> ok;
go_all_actors_part_b([A1 | T], Nodes_to_actors, No_actors, Output_file_path, Global_customer) ->
    A1 ! {go, Nodes_to_actors, No_actors, Output_file_path, Global_customer}, 
    go_all_actors_part_b(T, Nodes_to_actors, No_actors, Output_file_path, Global_customer).

    
start(Input_file_path, Part_a_output_file_path, Part_b_output_file_path) ->
    % Open files and parse input
    Input_text = readlines(Input_file_path),
    % Split input into lines
    Lines = string:tokens(Input_text, "\n"),
    Text_for_each_partition = groups_of_four(Lines, 1),
    % Send input text to parse function 
    Partitions = parse_each(Text_for_each_partition, 1),
    % Part a

    % Spawn actors for each partition
    Stats_cell = spawn(graph_stats, cell_part_a, [dict:new(), 0, length(Text_for_each_partition), Part_a_output_file_path]),
    spawn_actors_part_a(Partitions, Stats_cell),

    % Part b
    % clear file content    
    file:write_file(Part_b_output_file_path, ""),
    % Spawn actor for each partition, forming dictionary of nodes to actors
    Nodes_to_actors = spawn_actors_part_b(Partitions, dict:new()),
    % Get all actors
    Actors = sets:to_list(sets:from_list(second_elem(dict:to_list(Nodes_to_actors)))),
    % Use part_b_util to find influential nodes in each actor
    Global_customer = spawn(graph_stats, global_inf_nodes_cell, [[], 0, length(Actors), Part_b_output_file_path]),
    go_all_actors_part_b(Actors, Nodes_to_actors,length(Actors), Part_b_output_file_path, Global_customer).
%Alexander Manske & Fredrik Liljedahl
%manske@kth.se 		fliljeda@kth.se

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FIB %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fib(0, A,_, A). %Base case, when we reach N = 0, return.
%Tail recursion. Use N as counter, iterate until base value (N=0) is reached.
fib(N, A, B, F) :- N > 0, Nnew is N - 1, Sum is (A + B), fib(Nnew, B, Sum, F).
fib(N, F) :- fib(N, 0, 1, F). %set start values for when fib(N,F). is called

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ROVARSPRAK %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isVowel(Char) :- memberchk(Char, [97,101,105,111,117,121]). 
%memberchk = see if Char is an element in the given set.
rovarsprak([Char|Tail1],[Char|Tail2]):-
        isVowel(Char), %If we reached a vowel, just continue with the tail recursion
        rovarsprak(Tail1,Tail2),
        !.
rovarsprak([Char|Tail1],[Char,111,Char|Tail2]):-
        rovarsprak(Tail1,Tail2),
        !.
rovarsprak([],[]).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MEDELLANGD %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

medellangd([],0):- !. % Empty-string input yields a 0.

medellangd(Text, AvgLen) :- %User input
	run_algoritm(Text, Number_chars, Number_words),
	AvgLen is Number_chars/Number_words. %returs a floating-point number

run_algoritm([], 0, 0):- !. % Base case

run_algoritm([Fst,Snd|Tail], Chars, Words):- %Check if the two first characters are letters
	char_type(Fst, alpha),
	char_type(Snd, alpha), %Two letters in a row = same word.
	run_algoritm([Snd|Tail], Add_char, Words), !, %Step through to the next iteration.
	Chars is Add_char + 1. %So, add 1 to letters

run_algoritm([Head|Tail], Chars, Words):- %If we get here, the second character was not a letter.
	char_type(Head, alpha), %If the first is a letter, and we know the second is not, the word will end.
	run_algoritm(Tail, Add_char, Add_word), !,
	Chars is Add_char + 1, %So, we add 1 to characters,
	Words is Add_word + 1. %And 1 to words.

run_algoritm([_|Tail], Chars, Words) :- %If we get here, we had two non-letters in a row
	run_algoritm(Tail, Chars, Words). % So we just continue on.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKYFFLA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
skyffla([],[]) :- !. %Base case
skyffla(Lista, Skyfflad) :- 
	even(Lista, Even), % If index is even, save to Even
	odd(Lista, Odd), % If index is odd, save to Odd
	skyffla(Odd, Skyfflad2), % Call skyffla with the new list of odd indices
	append(Even, Skyfflad2, Skyfflad). % append even lists with odd lists recursively 
	


even([],[]) :- !. %base case
even([A],[A]) :- !. %Base case
even([Fst,_|Rest1],[Fst|Rest2]) :-
	even(Rest1,Rest2). % Extract the elements with even indices

odd([],[]):-!. % Base case
odd([_],[]) :- !. % Base case
odd([_,Snd|Rest1],[Snd|Rest2]) :-
	odd(Rest1,Rest2). % Extract the elements with odd indices
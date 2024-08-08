
:- [hogwarts].

studentOf(Student, Teacher) :-
    teacherOf(Teacher, Student).

classmates(StudentOne, StudentTwo):- 
    studentOf(StudentOne, X), studentOf(StudentTwo, X), StudentOne \= StudentTwo.

liveFarAway(StudentOne, StudentTwo):- 
    houseOf(Y, StudentOne), 
    houseOf(Z, StudentTwo),
    farLocation(Y, Z), 
    StudentOne \= StudentTwo.

isSeniorOf(PersonA, PersonB):- 
    directSeniorOf(PersonA, PersonB).

isSeniorOf(PersonA, PersonB) :- 
    directSeniorOf(PersonA, PersonC), 
    isSeniorOf(PersonC, PersonB).

listSeniors(Person, Seniors):-
    findall(X, isSeniorOf(X, Person), Seniors),
    write(Seniors).

listJuniors(Person, Juniors):- 
    findall(X, isSeniorOf(Person, X), Juniors),
    write(Juniors).

oldestStudent(Person, House) :- 
    houseOf(House, Person),
    birthYear(Person, Year),
    \+ (houseOf(House, X), birthYear(X, Y), Y < Year).  

youngestStudent(Person, House):-
    houseOf(House, Person),
    birthYear(Person, Year),
    \+ (houseOf(House, X), birthYear(X, Y), Y > Year).  

oldestQuidditchStudent(Team, Student):- 
    quidditchTeamOf(Team, Student),
    birthYear(Student, Year),
    \+ (quidditchTeamOf(Team, X), birthYear(X, Y), Y < Year).  

youngestQuidditchStudent(Team, Student):-
    quidditchTeamOf(Team, Student),
    birthYear(Student, Year),
    \+ (quidditchTeamOf(Team, X), birthYear(X, Y), Y > Year).  

rival(StudentOne, StudentTwo):- 
    houseOf(X, StudentOne),
    houseOf(Y, StudentTwo),
    StudentOne \= StudentTwo,
    X \= Y.

farRival(StudentOne, StudentTwo):-
    houseOf(X, StudentOne),
    houseOf(Y, StudentTwo),
    StudentOne \= StudentTwo,
    X \= Y,
    farLocation(X, Y).

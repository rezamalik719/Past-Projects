

Features
    Part 1

        studentOf(Student, Teacher) - 
            true if Student has Teacher as an instructor; false otherwise
        
        classmates(StudentOne, StudentTwo) -
            true if StudentOne and StudentTwo are distinct and share a class

        liveFarAway(StudentOne, StudentTwo) -
            true if StudentOne and StudentTwo are in different houses
                and those houses are far away from each geographically

        isSeniorOf(PersonA, PersonB) -
            true if PersonA is senior to personB; achieved by recursively
                leveraging directSeniorOf(PersonA, PersonB)

        listSeniors(Person, Seniors) -
            finds all seniors to Person and stores them in Seniors (also
                writes Seniors to console)

        listJuniors(Person, Juniors) -
            finds all seniors to Person and stores them in Juniors (also
                writes Juniors to console)

        oldestStudent(Person, House) -
            true if Person is the oldest person in the given House
        
        youngestStudent(Person, House) -
            true if Person is the youngest person in the given House

        oldestQuidditchStudent(Team, Student) -
            true if Student is the oldest member of the given house's quidditch team

        youngestQuidditchStudent(Team, Student) -
            true if Student is the youngest member of the given house's quidditch team

        rival(StudentOne, StudentTwo) -
            true if StudentOne and StudentTwo are distinct and from different houses

        rival(StudentOne, StudentTwo) -
            true if StudentOne and StudentTwo are distinct, from different houses,
                and those houses in geographically far from each other

    Part 2
        
        -- Parses a list of items belonging to a set of students from Hogwarts
            and a price/volume constraint for one or more of the 4 houses using
            a naive NLP approach.

        -- Solves the constraints for each house (e.g. < 600 dollars and > 50 cubic feet)
            by generating all possible combinations of items and seeing which combinations
            adhere to the constraints.

        -- Write all satisfactory combinations to the console for each house, listing
            the total price, total volume, and names of all adherent items for each
            valid combination.




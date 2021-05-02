package main;

import se.sics.jasper.*;

import java.io.File;
import java.util.*;

public class Simple
{
    public static void main(String[] args) {
        // read lines from predicate_definitions.txt and generate test queries
        SICStus sp;
        Query generateArgsQuery;
        HashMap<String, SPTerm> argsWayMap = new HashMap<>();
        List<String> testQueries;

        try {
            File predicateDefinitions = new File("predicate_definitions.txt");
            Scanner scanner = new Scanner(predicateDefinitions);

            sp = new SICStus();
            sp.restore("generators.sav");

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] split = line.split(";");
                String predicate = split[0];
                String argsRestrictions = split[1];

                generateArgsQuery = sp.openPrologQuery(String.format("input_gen(I, %s).", argsRestrictions), argsWayMap);

                try {
                    generateArgsQuery.nextSolution();
                    List<List<String>> generatedArgs = spTermToListOfLists(argsWayMap.get("I").toString());
                    System.out.println("Generated Arguments");
                    System.out.println(generatedArgs.toString() + "\n");

                    testQueries = fillArguments(predicate, generatedArgs);
                    System.out.println("Test Queries");
                    for (String testQuery : testQueries) {
                        System.out.println(testQuery);
                    }

                } catch ( Exception e ) {
                    System.out.println(e.toString());

//                    if (!e.toString().contains("permission_error")) {
//                        System.out.println(e.toString());
//                    }
                } finally {
                    generateArgsQuery.close();
                }
            }
            scanner.close();
        } catch (Exception e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }

        // iterate through all test queries and print SLD tree
        if (args.length < 1) {
            System.out.println("Please enter a test query");
            return;
        }
        Query modelQuery;
        HashMap<String, SPTerm> modelWayMap = new HashMap<>();
        try {
            sp = new SICStus();
            sp.restore("compare_dynamic.sav");
            String testQuery = args[0];
            if (testQuery.charAt(testQuery.length()-1) == '.') {
                testQuery = testQuery.substring(0, testQuery.length()-1);
            }

            modelQuery = sp.openPrologQuery(String.format("solve(model:%s, P).", testQuery), modelWayMap);
            try {
                while (modelQuery.nextSolution()) {
                    List<List<String>> modelSLD = spTermToListOfLists(modelWayMap.get("P").toString());
                    System.out.println(modelSLD.toString() + "\n");
                }
            } catch ( Exception e ) {
                if (!e.toString().contains("permission_error")) {
                    System.out.println(e.toString());
                }
            } finally {
                modelQuery.close();
            }
        } catch ( Exception e ) {
            e.printStackTrace();
        }
    }

    private static List<String> fillArguments(String predicate, List<List<String>> generatedArgs) {
        char variable = 'A';
        while (predicate.indexOf('-') != -1) {
            predicate = predicate.replaceFirst("-", String.valueOf(variable));
            variable++;
            System.out.println(predicate);
        }


        List<String> predicates = Collections.nCopies(1, predicate);
//        String predicate_backup = predicate;
        for (List<String> generatedArg : generatedArgs) {
            List<String> new_predicates = new ArrayList<>();

            for (String partial_predicate : predicates) {
                for (String arg : generatedArg) {
                    new_predicates.add(partial_predicate.replaceFirst("\\+", arg));
                }
            }

            predicates = new ArrayList<>(new_predicates);
        }

        return predicates;
    }

    public static List<String> splitIntoArray(String input) {
        // splits a string into an array of string based on commas which are not enclosed in quotes

        // remove initial and last []
        if (input.charAt(0) == '[' && input.charAt(input.length()-1) == ']') {
            input = input.substring(1, input.length()-1);
        }

        int start = 0;
        int precedingQuotes = 0;
        Set<Character> openingQuotes = new HashSet<>(Arrays.asList('(', '['));
        Set<Character> closingQuotes = new HashSet<>(Arrays.asList(')', ']'));
        List<String> stringArrayList = new ArrayList<>();

        for (int current = 0; current < input.length(); current++) {
            if (openingQuotes.contains(input.charAt(current))) {
                precedingQuotes++;
            } else if (closingQuotes.contains(input.charAt(current))) {
                precedingQuotes--;
            } else if (input.charAt(current) == ',' && precedingQuotes == 0) {
                stringArrayList.add(input.substring(start, current));
                start = current + 1;
            }
        }
        stringArrayList.add(input.substring(start));

        return stringArrayList;
    }

    public static List<List<String>> spTermToListOfLists(String spTerm) {
        String sld = spTermToListOfListsString(spTerm);
        List<String> subResults = splitIntoArray(sld);

        List<List<String>> result = new ArrayList<>();
        for (String layer : subResults) {
            result.add(splitIntoArray(layer));
        }

        return result;
    }

    public static String spTermToListOfListsString(String spTerm) {
        int squareCount = 0;
        int roundCount = 0;
        int tupleCount = 0;

        boolean endOfList = false;
        boolean endOfTuple = false;

        StringBuilder result = new StringBuilder();
        for (int i = 0; i < spTerm.length(); i++) {
            if (spTerm.charAt(i) == ',' && spTerm.charAt(i+1) == '.' && spTerm.charAt(i+2) == '(') {
                // replace ,.( which means inside a list with , seperator
                endOfList = false;
                endOfTuple = false;
                result.append(',');
                roundCount++;
                i += 2;
            } else if (spTerm.charAt(i) == '.' && spTerm.charAt(i+1) == '(') {
                // replace .( with start of list [
                endOfList = false;
                endOfTuple = false;
                result.append('[');
                squareCount++;
                i++;
            } else if (spTerm.charAt(i) == ',' && spTerm.charAt(i+1) == '(') {
                // replace tuples with empty
                endOfList = false;
                endOfTuple = false;
                tupleCount++;
                i++;
            } else if (spTerm.charAt(i) == ')' && roundCount > 0 && endOfList) {
                // remove excess ) at end of a list
                roundCount--;
            } else if (spTerm.charAt(i) == ')' && tupleCount > 0 && endOfTuple) {
                // remove excess ) at end of a tuple
                tupleCount--;
            } else if (spTerm.charAt(i) == ')' && spTerm.charAt(i+1) == ')') {
                /*
                TODO: Might not be true
                is at end of tuple
                */
                endOfTuple = true;
                result.append(')');
            } else if (spTerm.charAt(i) == ',' && spTerm.charAt(i+1) == '[' && spTerm.charAt(i+2) == ']'
                       && spTerm.charAt(i+3) == ')' && squareCount > 0) {
                // flag for when at end of list, replace with ]
                endOfList = true;
                endOfTuple = false;
                result.append(']');
                squareCount--;
                i += 3;
            } else {
                endOfList = false;
                endOfTuple = false;
                result.append(spTerm.charAt(i));
            }
        }
//        System.out.println(roundCount);
//        System.out.println(squareCount);
//        System.out.println(tupleCount);

        return result.toString();
    }
}
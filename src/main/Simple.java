package main;

import se.sics.jasper.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class Simple
{
    public static void main(String[] args) {
//        // for testing only
//
//        try {
//            SICStus sp = new SICStus();
//            sp.restore("compare_dynamic.sav");
//
//            HashMap<String, SPTerm> wayMap = new HashMap<>();
//            Query query = sp.openPrologQuery(String.format("solve(%s:%s, P).", "student", "safe([[h],[z]])"), wayMap);
//
//            try {
//                while (query.nextSolution()) {
//                    System.out.println(wayMap.get("P").toString());
//                    System.out.println(spTermToListOfLists(wayMap.get("P")));
//                }
//            } catch ( Exception e ) {
//                if (!e.toString().contains("permission_error")) {
//                    System.out.println(e.toString());
//                }
//            } finally {
//                query.close();
//            }
//        } catch (Exception e) {
//            e.printStackTrace();
//        }

        // read lines from predicate_definitions.txt and generate test queries
        // then, pass them to get SLD trees

        // First, read from file
        File predicateDefinitions = new File("predicate_definitions.txt");
        Scanner scanner = null;
        try {
            scanner = new Scanner(predicateDefinitions);
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        }

        try {
            // initialise SICStus objects, used to query predicates
            SICStus sp_generator = new SICStus();
            sp_generator.restore("generators.sav");

            SICStus sp_compare = new SICStus();
            sp_compare.restore("compare_dynamic.sav");

            assert scanner != null;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] split = line.split(";");

                String predicate = split[0];
                String argsRestrictions = split[1];
                String optionalArgsRestrictions = "";
                if (split.length == 3) {
                    optionalArgsRestrictions = split[2];
                }

                // generate args for + inputs
                List<List<String>> generatedArgs = generateArgs(sp_generator, argsRestrictions);
                System.out.println("Generated Arguments");
                System.out.println(generatedArgs.toString() + "\n");

                // generate args for ? inputs, if applicable
                List<List<String>> optionalGeneratedArgs = null;
                if (!optionalArgsRestrictions.isEmpty()) {
                    optionalGeneratedArgs = generateArgs(sp_generator, optionalArgsRestrictions);
                    System.out.println("Generated Arguments");
                    System.out.println(optionalGeneratedArgs.toString() + "\n");
                }

                // fill predicate with generated args
                List<String> testQueries = fillArguments(predicate, generatedArgs);
                if (optionalGeneratedArgs != null) {
                    testQueries = fillArgumentsQuestion(testQueries, optionalGeneratedArgs);
                }

                // compare test queries
                System.out.println("=====Test Queries=====");
                compareTestQueries(sp_compare, testQueries);
                System.out.println("===End Test Queries===");
                System.out.println("");

            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void compareTestQueries(SICStus sp_compare, List<String> testQueries) throws Exception {

        for (String testQuery : testQueries) {
            // don't compare queries starting with test
            if (testQuery.substring(0, 4).equals("test")) {
                continue;
            }

            // remove . at end of predicates
            if (testQuery.charAt(testQuery.length()-1) == '.') {
                testQuery = testQuery.substring(0, testQuery.length()-1);
            }

            List<List<List<String>>> modelSLDTrees = getSLDTrees(sp_compare, testQuery, "model");
            List<List<List<String>>> studentSLDTrees = getSLDTrees(sp_compare, testQuery, "student");

            // TODO: semantic comparison of SLD Trees

            // Length comparison, see if student answer is over or under determined
            // TODO: instead of trim compare most similar?
            if (studentSLDTrees.size() > modelSLDTrees.size()) {
                StringBuilder response = new StringBuilder();
                response.append("Too many answers. Comparing first ");
                response.append(modelSLDTrees.size());
                response.append(" answers, ignoring last ");
                response.append(studentSLDTrees.size() - modelSLDTrees.size());
                response.append(" answers from student implementation. ");

                response.append("Consider using cuts (!/2) to optimise your code.");

                System.out.println(response.toString());

                studentSLDTrees = studentSLDTrees.subList(0, modelSLDTrees.size());

            } else if (studentSLDTrees.size() < modelSLDTrees.size()) {
                StringBuilder response = new StringBuilder();
                response.append("Too few answers. Comparing first ");
                response.append(studentSLDTrees.size());
                response.append(" answers, ignoring last ");
                response.append(modelSLDTrees.size() - studentSLDTrees.size());
                response.append(" answers from model implementation. ");

                response.append("Consider removing cuts or adding additional predicates.");

                System.out.println(response.toString());

                modelSLDTrees = modelSLDTrees.subList(0, studentSLDTrees.size());
            }

            for (int i = 0; i < modelSLDTrees.size(); i++) {
                List<List<String>> modelSLDTree = modelSLDTrees.get(i);
                List<List<String>> studentSLDTree = studentSLDTrees.get(i);
                semanticComparison(modelSLDTree, studentSLDTree);
            }

            System.out.println("");
        }
    }

    private static void semanticComparison(List<List<String>> modelSLDTree, List<List<String>> studentSLDTree) {
        if (modelSLDTree.equals(studentSLDTree)) {
            return;
        }

        // TODO: compare just length of SLD trees


        // iterate through each layer of studentSLDTree and compare to
        // layers in modelSLDTree
        for (int i = 0; i < studentSLDTree.size(); i++) {
            List<String> layer = studentSLDTree.get(i);

            // skip last resolution if it is [true]
            if (layer.toString().equals("[true]")) {
                continue;
            }

            // TODO: increase similarity score, etc.
            if (layer.equals(modelSLDTree.get(i))) {
                continue;
            }

            // returns -1 if does not exist
            int modelIndex = modelSLDTree.indexOf(layer);

            if (modelIndex == -1) {
                // TODO: more in depth comparison
                System.out.println(layer + " is not found in the model SLD tree.");
            } else {
                if (modelIndex > i) {
                    System.out.println(layer + " occurred too early.");
                } else if (modelIndex < i) {
                    System.out.println(layer + " occurred too late.");
                }
            }
        }
    }

    private static List<List<List<String>>> getSLDTrees(SICStus sp_compare, String testQuery, String module) throws Exception {
        List<List<List<String>>> SLDTrees = new ArrayList<>();
        HashMap<String, SPTerm> wayMap = new HashMap<>();
        Query query = sp_compare.openPrologQuery(String.format("solve(%s:%s, P).", module, testQuery), wayMap);
        System.out.println("Solving " + module + ":" + testQuery);

        try {
            while (query.nextSolution()) {
                List<List<String>> modelSLD = spTermToListOfLists(wayMap.get("P"));
                SLDTrees.add(modelSLD);
                System.out.println(module + ": " + modelSLD.toString());
            }
        } catch ( Exception e ) {
            if (!e.toString().contains("permission_error")) {
                System.out.println(e.toString());
            }
        } finally {
            query.close();
        }
        return SLDTrees;
    }

    private static List<List<String>> generateArgs(SICStus sp_generator, String argsRestrictions) {
        Query generateArgsQuery;
        List<List<String>> generatedArgs = null;

        try {
            HashMap<String, SPTerm> argsWayMap = new HashMap<>();
            generateArgsQuery = sp_generator.openPrologQuery(String.format("input_gen(I, %s).",
                    argsRestrictions), argsWayMap);
            try {
                generateArgsQuery.nextSolution();
                generatedArgs = spTermToListOfLists(argsWayMap.get("I"));
            } catch ( Exception e ) {
                System.out.println(e.toString());
            } finally {
                generateArgsQuery.close();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return generatedArgs;
    }

    private static List<String> fillArgumentsQuestion(List<String> testQueriesWithQuestion,
                                                      List<List<String>> optionalGeneratedArgs) {
        List<String> testQueries = new ArrayList<>();
        char variable = 'A';

        while (testQueriesWithQuestion.get(0).indexOf('?') != -1) {
            // start fresh
            testQueries = new ArrayList<>();

            for (String testQueryWithQuestion : testQueriesWithQuestion) {
                int index = testQueryWithQuestion.indexOf('?');
                // replace ? with a variable, AA to AZ supported
                testQueries.add(testQueryWithQuestion.substring(0, index) + 'A' + variable + testQueryWithQuestion.substring(index+1));

                // replace ? with possible arguments
                for (String arg : optionalGeneratedArgs.get(0)) {
                    testQueries.add(testQueryWithQuestion.substring(0, index) + arg + testQueryWithQuestion.substring(index+1));
                }
            }

            // remove used args and increment variable
            variable++;
            optionalGeneratedArgs.remove(0);

            testQueriesWithQuestion = new ArrayList<>(testQueries);
        }
        return testQueries;
    }

    private static List<String> fillArguments(String predicate,
                                              List<List<String>> generatedArgs) {

        // replace all '-' with unique capital letters (limited from A to Z)
        char variable = 'A';
        while (predicate.indexOf('-') != -1) {
            predicate = predicate.replaceFirst("-", String.valueOf(variable));
            variable++;
        }

        // replace all '+' with all possible combinations of the generatedArgs
        List<String> predicates = Collections.nCopies(1, predicate);
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

    private static String spTermToString(String spTerm) {
        int squareCount = 0;
        int roundCount = 0;
        int tupleCount = 0;

        boolean endOfList = false;
        boolean endOfTuple = false;

        StringBuilder result = new StringBuilder();
        int n = spTerm.length();

        for (int i = 0; i < n; i++) {
            if (i+2 < n && spTerm.charAt(i) == ',' && spTerm.charAt(i+1) == '.' && spTerm.charAt(i+2) == '(') {
                // replace ,.( which means inside a list with , seperator
                endOfList = false;
                endOfTuple = false;
                result.append(',');
                roundCount++;
                i += 2;
            } else if (i+1 < n && spTerm.charAt(i) == '.' && spTerm.charAt(i+1) == '(') {
                // replace .( with start of list [
                endOfList = false;
                endOfTuple = false;
                result.append('[');
                squareCount++;
                i++;
            } else if (i+1 < n && spTerm.charAt(i) == ',' && spTerm.charAt(i+1) == '(') {
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
            } else if (i+1 < n && spTerm.charAt(i) == ')' && spTerm.charAt(i+1) == ')') {
                /*
                TODO: Might not be true
                is at end of tuple
                */
                endOfTuple = true;
                result.append(')');
            } else if (i+3 < n && spTerm.charAt(i) == ',' && spTerm.charAt(i+1) == '[' && spTerm.charAt(i+2) == ']'
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
        return result.toString();
    }

    private static List<List<String>> spTermToListOfLists(SPTerm spTerm) {
        List<List<String>> result = new ArrayList<>();

        try {
            SPTerm[] list = spTerm.toTermArray();

            for (SPTerm layer : list) {
                List<String> innerListString = new ArrayList<>();
                SPTerm[] inner = layer.toTermArray();
                
                for (SPTerm term : inner) {
                    String termString = spTermToString(term.toString());
                    innerListString.add(termString);
                }

                result.add(innerListString);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return result;
    }
}
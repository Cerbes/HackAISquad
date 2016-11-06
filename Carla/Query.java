import java.util.*;
import java.io.File; //for file haddling
import java.io.PrintWriter; //for output objects

public class Query{
    public static void main(String[] args) throws Exception {

        Scanner input = new Scanner(System.in); //create Scanner object for input
        System.out.print("Snake Interpreter\n"); //title

        boolean again=false;

        do {// run as long as the user wants to  look up snakes
            boolean  error=false;
            String hold, head, rattle, length;
            String[] pat, col, snakes;
            ArrayList<String> pattern, color;

            System.out.print("Color Options: \n"); //gets colors from user
            System.out.print("A.red B.yellow C.orange D.brown E.beige F.black G.grey H.white I.blue\n");
            col = getColorArray(input);

            System.out.print("Pattern Options: \n"); //gets patterns from user
            System.out.print("A.small spots B.large spots C.horizontal stripes D.vertical stripes E.gradient F.other\n");
            pat = getPatternArray(input);

            System.out.print("Head Options: \n"); //head options
            System.out.print("A.big head B.small head\n");
            head = getHead(input);


            System.out.print("Rattle Options: \n"); //rattle options
            System.out.print("A.yes B.no\n");
            rattle = getRattle(input);

            System.out.print("length Options (in meters): \n"); //length options
            System.out.print("A.large(greater than 2) B.medium(1-2) C.small(less than 1)\n");
            length = getLength(input);

            printSasp(length, head, rattle, pat, col);  //writes query.lp

            //opening prolog and sending output to correct file

            //this if reading prolog's output

            //printSaps2(snakes);

            //more stuff about running prolog

            //getting if posionous from output

            //telling the user if it is posionous

        } while(again);


    }

    public static String[] getColorArray(Scanner input){ //gets valid input for color array
        boolean error, black, brown;
        String hold;
        String[] col;
        ArrayList<String> color;
        do{ //color
            error = false; black = false; brown = false;
            System.out.print("Enter the letters of all that apply (seperate by spaces): ");
            hold = input.nextLine();
            col = hold.split("\\s+");
            color = new ArrayList<String>(Arrays.asList(col));
            for (int i = 0; i < color.size(); i++) {// not brown and not black
                if((color.get(i)).equals("A"))
                    color.set(i, "red");
                else if((color.get(i)).equals("B"))
                    color.set(i, "yellow");
                else if((color.get(i)).equals("C"))
                    color.set(i, "orange");
                else if((color.get(i)).equals("D"))
                    color.set(i, "brown");
                else if((color.get(i)).equals("E"))
                    color.set(i, "beige");
                else if((color.get(i)).equals("F"))
                    color.set(i, "black");
                else if((color.get(i)).equals("G"))
                    color.set(i, "grey");
                else if((color.get(i)).equals("H"))
                    color.set(i, "white");
                else if((color.get(i)).equals("I"))
                    color.set(i, "blue");
                else{
                    error=true;
                    System.out.print("ERROR!Please try again.");
                    break;
                }

                if((color.get(i)).equals("black"))
                    black = true;
                if((color.get(i)).equals("brown"))
                    brown = true;

            }

            if(!error){
                color.trimToSize();
                if(!black)
                    color.add(color.size(), "not_black");
                if(!brown)
                    color.add(color.size(), "not_brown");
            }

        }while(error);

        col = color.toArray(new String[color.size()]);
        return col;
    }

    public static String[] getPatternArray(Scanner input){
        boolean error;
        String hold;
        String[] pat;
        ArrayList<String> pattern;
        do{
            error = false;
            System.out.print("Enter the letters of all that apply (seperate by spaces): ");
            hold = input.nextLine();
            pat = hold.split("\\s+");
            pattern = new ArrayList<String>(Arrays.asList(pat));
            for (int i = 0; i < pattern.size(); i++) {// not brown and not black
                if((pattern.get(i)).equals("A"))
                    pattern.set(i, "small_spotted");
                else if((pattern.get(i)).equals("B"))
                    pattern.set(i, "large_spotted");
                else if((pattern.get(i)).equals("C"))
                    pattern.set(i, "horizontal_striped");
                else if((pattern.get(i)).equals("D"))
                    pattern.set(i, "vertical_striped");
                else if((pattern.get(i)).equals("E"))
                    pattern.set(i, "gradient");
                else if((pattern.get(i)).equals("F"))
                    pattern.set(i, "no_pattern");
                else{
                    error=true;
                    System.out.print("ERROR!Please try again.");
                    break;
                }
            }

        }while(error);

        pat = pattern.toArray(new String[pattern.size()]);
        return pat;
    }

    public static String getHead(Scanner input){
        boolean error;
        String head;
        do{
            error = false;
            System.out.print("Enter the letter of the choice that applies: ");
            head = input.next();
            if(head.equals("A"))
                head = "big_head";
            else if(head.equals("B"))
                head = "small_head";
            else{
                error=true;
                System.out.print("ERROR!Please try again.");
            }
        }while(error);
        return head;
    }

    public static String getRattle(Scanner input){
        boolean error;
        String rattle;
        do{
            error = false;
            System.out.print("Enter the letter of the choice that applies: ");
            rattle = input.next();
            if(rattle.equals("A"))
                rattle = "rattle_yes";
            else if(rattle.equals("B"))
                rattle = "rattle_no";
            else{
                error=true;
                System.out.print("ERROR!Please try again.");
            }

        }while(error);
        return rattle;
    }

    public static String getLength(Scanner input){
        boolean error;
        String length;
        do{
            error = false;
            System.out.print("Enter the letter of the choice that applies: ");
            length = input.next();
            if(length.equals("A"))
                length = "large";
            else if(length.equals("B"))
                length = "medium";
            else if(length.equals("C"))
                length = "small";
            else{
                error=true;
                System.out.print("ERROR!Please try again.");
            }
        }while(error);
        return length;
    }

    public static void printSasp(String length, String head, String rattle, String[] Pattern, String[] Colors) throws Exception{
        String filename;
        filename = "query.lp"; //create filename to open files with
        File file = new File(filename); //create file object
        try(PrintWriter output = new PrintWriter(file);){ //create scanner object in try block to that it closes the file automatically
            output.print("#compute 4 {" + length + "(X), " + head + "(X), " + rattle + "(X)");
            for (String e: Pattern) {
                output.print(", " + e + "(X)");
            }
            for (String e: Colors) {
                output.print(", " + e + "(X)");
            }
            output.print("}.");
        }
    }
/*
    public static printSaps2(String[] snakes){
        String filename;
        filename = "query.lp"; //create filename to open files with
        File file = new File(filename); //create file object
        try(PrintWriter output = new PrintWriter(file);){ //create scanner object in try block to that it closes the file automatically
            output.print("");
            for (String e: Pattern) {
                output.println("?- venomous(" + e + ").");
                output.println("?- mild_venomous(" + e + ").");
            }


        }
    }*/

}

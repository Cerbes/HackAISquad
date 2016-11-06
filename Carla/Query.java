import java.util.*;
import java.io.File; //for file haddling
import java.io.PrintWriter;

public class Query{
    public static void main(String[] args) throws Exception {

        Scanner input = new Scanner(System.in);
        System.out.print("Snake Interpreter\n");

        boolean again=false;

        do {// run as long as the user wants to  look up snakes
            boolean  error=false;
            String hold, head, rattle, length;
            String[] pat, col;
            ArrayList<String> pattern, color;

            System.out.print("Color Options: \n");
            System.out.print("A.red B.yellow C.orange D.brown E.beige F.black G.grey H.white I.blue\n");

            do{ //color
                error = false;
                boolean black = false, brown = false;
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

            System.out.print("Pattern Options: \n");
            System.out.print("A.small spots B.large spots C.horizontal stripes D.vertical stripes E.gradient F.other\n");
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

            System.out.print("Head Options: \n"); //head options
            System.out.print("A.big head B.small head\n");
            do{
                error = false;
                System.out.print("Enter the letter of the choice that applys: ");
                head = input.next();
                if(head.equals("A"))
                    head = "big_head";
                else if(head.equals("B"))
                    head = "small_head";
                else{
                    error=true;
                    System.out.print("ERROR!Please try again.");
                    break;
                }

            }while(error);

            System.out.print("Rattle Options: \n"); //head options
            System.out.print("A.yes B.no\n");
            do{
                error = false;
                System.out.print("Enter the letter of the choice that applys: ");
                rattle = input.next();
                if(rattle.equals("A"))
                    rattle = "rattle_yes";
                else if(rattle.equals("B"))
                    rattle = "rattle_no";
                else{
                    error=true;
                    System.out.print("ERROR!Please try again.");
                    break;
                }

            }while(error);

            System.out.print("length Options (in meters): \n"); //head options
            System.out.print("A.large(greater than 2) B.medium(1-2) C.small(less than 1)\n");
            do{
                error = false;
                System.out.print("Enter the letter of the choice that applys: ");
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
                    break;
                }
            }while(error);

            printSasp(length, head, rattle, pat, col);  //writes query.lp

        } while(again);


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

}

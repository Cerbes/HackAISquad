package snakeIdentificationProgram;

import java.io.FileReader;
import java.io.IOException;

public class ReadVenomous {
	public static void call() {
	//read in file
		String[] snakeArray = new String[5]; // array to store 
			try {
				FileReader reader = new FileReader("venomOutput.txt");
				int character;
				boolean flag= false;
				String snakeName="";
				int i=0; // iteration for array
				
				while ((character = reader.read()) != -1) {
					// found snake make flag true
					if((char) character =='('){
						character = reader.read();
						flag =true;
					}
					// snake name ends and input into array 
					if((char) character ==')'){
						flag=false;
						snakeArray[i]=snakeName; //stores into array 
						i++; // iteration for array
						//reseting strings
						snakeName="";
						//temp="";
					}
					//keep reading snake
					if (flag==true){
						snakeName+=(char) character;
						//System.out.print((char) character);
					}

				}
				reader.close();
				snakeArray[i]="false";
				System.out.println("\nVenomous snakes:");
				// print snakes
				String[] name;
				char upper;
				String tempName;
				if(snakeArray[0].equals("false"))
					System.out.println("None");
				//print array
				else {
					for(int j=0;j<i;j++){
						name = snakeArray[j].split("_");
						tempName="";
						for(int x=0; x<name.length; x++){
							upper = Character.toUpperCase(name[x].charAt(0));
							tempName= tempName + upper + name[x].substring(1) + " ";
						}
						System.out.println(tempName);
					}
				}
				

			} catch (IOException e) {
				e.printStackTrace();
			}


		}
}


import java.io.FileReader;
import java.io.IOException;

public class Read1 {
	public static void main(String[] args) {
		//read in file
		try {
			FileReader reader = new FileReader("fileOutput.txt");
			int character;
			boolean flag= false;
			String snakeName="";
			String temp="";
			int i=0; // iteration for array
			String[] snakeArray = new String[5]; // array to store 
			while ((character = reader.read()) != -1) {
				// found snake make flag true
				if((char) character =='='){
					character = reader.read();
					flag =true;
				}
				// snake name ends and input into array 
				if((char) character =='.'){
					flag=false;
					temp= snakeName.replace("false", ""); // takes out unecceary words from prolog
					//System.out.println(temp); // print what goes into te array
					snakeArray[i]=temp; //stores into array 
					i++; // iteration for array
					//reseting strings
					snakeName="";
					temp="";
				}
				//keep reading snake
				if (flag==true){
					snakeName+=(char) character;
					//System.out.print((char) character);
				}

			}
			reader.close();
			/* print array
			for(int j=0;j<i;j++){
				System.out.println(snakeArray[j]);
			}
			*/

		} catch (IOException e) {
			e.printStackTrace();
		}


	}
}


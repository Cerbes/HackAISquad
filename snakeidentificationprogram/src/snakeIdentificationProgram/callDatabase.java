package snakeIdentificationProgram;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.RandomAccessFile;
import java.util.*;
import java.io.*;

public class callDatabase extends Thread {

	InputStream is;
    String type;
    OutputStream os;
    
    callDatabase(InputStream is, String type)
    {
        this(is, type, null);
    }
    callDatabase(InputStream is, String type, OutputStream redirect)
    {
        this.is = is;
        this.type = type;
        this.os = redirect;
    }
    
    public void run()
    {
        try
        {
            PrintWriter pw = null;
            if (os != null)
                pw = new PrintWriter(os);
                
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr);
            String line=null;
            while ( (line = br.readLine()) != null)
            {
                if (pw != null)
                    pw.println(line);
                System.out.println(type + ">" + line);    
            }
            if (pw != null)
                pw.flush();
        } catch (IOException ioe)
            {
            ioe.printStackTrace();  
            }
    }
}


/*
 * RandomAccessFile file = new RandomAccessFile("fileOutput.txt", "rw");
		//RandomAccessFile venomFile = new RandomAccessFile("venomOutput.txt", "rw");
		file.setLength(0);
		//venomFile.setLength(0);
		
		//Runtime rt = Runtime.getRuntime();
		//String[] testArgs = {"sasp", "snakeDatabase.lp"};
		//Process pr = rt.exec("sasp snakeDatabase.lp"); //execute finding snake command
		String command = "sasp snakeDatabase.lp";
		Process pr = Runtime.getRuntime().exec(command);
		BufferedReader r = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		
		//System.out.println(s);
		
		/*Runtime rt2 = Runtime.getRuntime();
		Process pr2 = rt2.exec(testArgs); //execute if venomous command
		BufferedReader r2 = new BufferedReader(new InputStreamReader(pr2.getInputStream()));
		
		s = null;
		
		/*while ((s=r2.readLine()) != null) {
			venomFile.writeBytes(s);
		}
		
		
		file.close();
		//venomFile.close();
 */

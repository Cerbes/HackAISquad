package snakeIdentificationProgram;

import java.io.*;
class StreamGobbler extends Thread
{
    InputStream is;
    String type;
    OutputStream os;
    
    StreamGobbler(InputStream is, String type)
    {
        this(is, type, null);
    }
    StreamGobbler(InputStream is, String type, OutputStream redirect)
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
                //System.out.println(type + ">" + line);    
            }
            if (pw != null)
                pw.flush();
        } catch (IOException ioe)
            {
            ioe.printStackTrace();  
            }
    }
}
public class GoodWinRedirect
{
    public static void call(int num)
    {
    	if (num == 1) {
	        String arg = "fileOutput.txt";
	        
	        try
	        {            
	            FileOutputStream fos = new FileOutputStream(arg);
	            Runtime rt = Runtime.getRuntime();
	            Process proc = rt.exec("sasp snakedatabase.lp");
	            // any error message?
	            StreamGobbler errorGobbler = new 
	                StreamGobbler(proc.getErrorStream(), "ERROR");            
	            
	            // any output?
	            StreamGobbler outputGobbler = new 
	                StreamGobbler(proc.getInputStream(), "OUTPUT", fos);
	                
	            // kick them off
	            errorGobbler.start();
	            outputGobbler.start();
	                                    
	            // any error???
	            int exitVal = proc.waitFor();
	            //System.out.println("ExitValue: " + exitVal);
	            fos.flush();
	            fos.close();        
	        } catch (Throwable t)
	          {
	            t.printStackTrace();
	          }
    	}
    	else {
    		String arg = "venomOutput.txt";
            
            try
            {            
                FileOutputStream fos = new FileOutputStream(arg);
                Runtime rt = Runtime.getRuntime();
                Process proc = rt.exec("sasp snakedatabase.lp");
                // any error message?
                StreamGobbler errorGobbler = new 
                    StreamGobbler(proc.getErrorStream(), "ERROR");            
                
                // any output?
                StreamGobbler outputGobbler = new 
                    StreamGobbler(proc.getInputStream(), "OUTPUT", fos);
                    
                // kick them off
                errorGobbler.start();
                outputGobbler.start();
                                        
                // any error???
                int exitVal = proc.waitFor();
                //System.out.println("ExitValue: " + exitVal);
                fos.flush();
                fos.close();        
            } catch (Throwable t)
              {
                t.printStackTrace();
              }
    	}
    }
}
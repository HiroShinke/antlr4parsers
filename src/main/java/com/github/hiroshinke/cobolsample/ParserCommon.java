

package com.github.hiroshinke.cobolsample;;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import java.util.Collection;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class ParserCommon {

    public interface Consumer<T> {
	void accept(T t) throws Exception;
    }

    public static void doFile(File file, Consumer<File> proc) throws Exception {

	if( file.isDirectory() ){
	    doDir(file,proc);
	}
	else {
	    try {
		proc.accept(file);
	    } catch( Exception e ){
		e.printStackTrace();
		throw e;
	    }
	}
    }

    public static void doDir(File file, Consumer<File> proc) throws Exception {

	for(File f: file.listFiles() ){
	    doFile(f,proc);
	}
    }


    static Pattern pattern = Pattern.compile("^.{6}-\\s+(\"|\')");

    public static InputStream toSrcStream(InputStream is) throws Exception {

	BufferedReader rd = new BufferedReader(new InputStreamReader(is));
	StringBuffer buff = new StringBuffer();

	String line;
	while( (line = rd.readLine()) != null ){

	    int len = line.length();
	    int cutoff = len < 72 ? len : 72;

	    if( len < 7 ){
		;
	    } else if( line.charAt(6) == ' ' ){
		buff.append(line.substring(7,cutoff));
		buff.append('\n');
	    }
	    else if( line.charAt(6) == '-' ){
		int last = buff.length();
		buff.delete(last-1,last);
		Matcher m = pattern.matcher(line);
		if( m.find() ){
		    int e = m.end();
		    buff.append(line.substring(e,cutoff));
		} else {
		    buff.append(line.substring(7,cutoff));
		}
		buff.append('\n');
	    }
	}
	return new ByteArrayInputStream(buff.toString().
					getBytes(StandardCharsets.UTF_8));
    }

}

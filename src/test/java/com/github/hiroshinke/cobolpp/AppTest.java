

package com.github.hiroshinke.cobolpp;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.Rule;
import static org.hamcrest.Matchers.is;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import java.util.Collection;
import java.util.List;

import org.antlr.v4.runtime.tree.xpath.XPath;
import org.antlr.v4.runtime.tree.ParseTree;

import static com.github.hiroshinke.cobolpp.App.*;
import static com.github.hiroshinke.cobolsample.AntlrUtil.nchar;

/**
 * Unit test for simple App.
 */
public class AppTest
{

    public InputStream toInputStream(String text) throws IOException {
	    return new ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8));
	}

    public BufferedReader bufferedReader(InputStream is){
	return new BufferedReader(new InputStreamReader(is));
    }

    public String fillToWidth(String text){
	StringBuffer buff = new StringBuffer();
	buff.append(text);
	buff.append(nchar(' ', 65 - text.length()));
	return buff.toString();
    }
    
    @Test
    public void testApp1() throws Exception 
    {
	InputStream is = preprocessStream(toInputStream("01 XXXX PIC 9(10). "));
	BufferedReader rd = bufferedReader(is);
	String src = rd.readLine();

	assertThat(src,is(fillToWidth("01 XXXX PIC 9(10). ")));
    }

    @Test
    public void testApp2() throws Exception 
    {
	InputStream is = preprocessStream
	    (toInputStream
	     (
	      "01 XXXX PIC \n" +
	      "            9(10). \n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 XXXX PIC ")));
	assertThat(src2,is(fillToWidth("            9(10). ")));
    }

    @Test
    public void testApp3() throws Exception 
    {
	InputStream is = preprocessStream
	    (toInputStream
	     (
	      "01 XXXX COPY YYYY.\n" +
	      "01 YYYY PIC X(10).\n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 XXXX")));
	assertThat(src2,is(fillToWidth("01 YYYY PIC X(10).")));	
    }
    
}

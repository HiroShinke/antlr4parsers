

package com.github.hiroshinke.cobolpp;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.Rule;
import org.junit.rules.TemporaryFolder;

import static org.hamcrest.Matchers.is;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.io.File;

import java.util.Collection;
import java.util.List;
import java.util.ArrayList;

import org.antlr.v4.runtime.tree.xpath.XPath;
import org.antlr.v4.runtime.tree.ParseTree;

import org.apache.commons.io.FileUtils;
import static com.github.hiroshinke.cobolsample.AntlrUtil.nchar;

import com.github.hiroshinke.cobolpp.CobolPreprocessor.SrcText;
import static com.github.hiroshinke.cobolpp.CobolPreprocessor.srcFromSrcTexts;

/**
 * Unit test for simple App.
 */
public class SrcTextTest
{

    CobolPreprocessor.SrcText st(String text,
	       int line,
	       int startPos){
	return new SrcText(text,line,startPos);
    }

    public String fillToWidth(String text){
	StringBuffer buff = new StringBuffer();
	buff.append(text);
	buff.append(nchar(' ', 65 - text.length()));
	return buff.toString();
    }

    ArrayList<SrcText> l(SrcText... text){
	ArrayList<SrcText> buff = new ArrayList<SrcText>();
	buff.addAll(List.of(text));
	return buff;
    }
    
    @Test
    public void testApp1() throws Exception 
    {
	assertThat(fillToWidth("A"),
		   is(srcFromSrcTexts(l(
					st("A",1,0)
					),
				      65)));
    }

    @Test
    public void testApp1_1() throws Exception 
    {
	assertThat(fillToWidth("   A"),
		   is(srcFromSrcTexts(l(
					st("A",1,3)
					),
				      65)));
    }

    
    @Test
    public void testApp2() throws Exception 
    {
	assertThat(fillToWidth("A B"),
		   is(srcFromSrcTexts(l(
					st("A",1,0), st("B",1,2)
					),
				      65)));
    }

    @Test
    public void testApp2_1() throws Exception 
    {
	assertThat(fillToWidth("AAB"),
		   is(srcFromSrcTexts(l(
					st("AA",1,0), st("B",1,2)
					),
				      65)));
    }

    @Test
    public void testApp2_2() throws Exception 
    {
	assertThat(fillToWidth("AAAB"),
		   is(srcFromSrcTexts(l(
					st("AAA",1,0), st("B",1,2)
					),
				      65)));
    }

    @Test
    public void testApp3() throws Exception 
    {
	assertThat(fillToWidth("A") + "\n" +
		   fillToWidth("B"),
		   is(srcFromSrcTexts(l(
					st("A",1,0), st("B",2,0)
					),
				      65)));
    }

    @Test
    public void testApp3_1() throws Exception 
    {
	assertThat("A\n" + 
		   fillToWidth("B"),
		   is(srcFromSrcTexts(l(
					st("A\n",1,0), st("B",2,0)
					),
				      65)));
    }

    

    @Test
    public void testApp3_2() throws Exception 
    {
	assertThat(fillToWidth("A") + "\n" +
		   fillToWidth("B"),
		   is(srcFromSrcTexts(l(
					st("A",1,0),
					st("\n",1,2),
					st("B",2,0)
					),
				      65)));
    }


    @Test
    public void testApp4() throws Exception 
    {
	assertThat(srcFromSrcTexts(l(
				     ),
				   65),
		   is(fillToWidth("")));
		   
    }
    
    
}

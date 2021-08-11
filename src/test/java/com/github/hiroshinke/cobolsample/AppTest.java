

package com.github.hiroshinke.cobolsample;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.Rule;
import static org.hamcrest.Matchers.is;
import java.io.IOException;
import java.io.InputStream;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import java.util.Collection;

import org.antlr.v4.runtime.tree.xpath.XPath;
import org.antlr.v4.runtime.tree.ParseTree;

/**
 * Unit test for simple App.
 */
public class AppTest
{

    public InputStream toInputStream(String text) throws IOException {
	    return new ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8));
	}
    
    @Test
    public void testApp1() throws Exception 
    {
	InputStream is = toInputStream("01 XXXX PIC 9(10). ");
	Cobol85Parser parser = App.createParser(is);
	ParseTree tree = parser.dataDescriptionEntry();
	String xpath = "//dataDescriptionEntry/*";
	String value = AntlrUtil.xpathSubTreesCont
	    (
	     tree,
	     xpath,
	     parser,
	     (entries) -> {
		 for(ParseTree e: entries){
		     String name2 = AntlrUtil.xpathSubTreeText
			 (e,
			  "*/dataName",
			  parser
			  );
		     return name2;
		 }
		 return null;
	     });
	assertThat(value,is("XXXX"));			     
    }

    @Test
    public void testApp2() throws Exception 
    {
	InputStream is = toInputStream("01 XXXX PIC 9(10). ");
	Cobol85Parser parser = App.createParser(is);
	ParseTree tree = parser.dataDescriptionEntry();
	String xpath = "//dataDescriptionEntry/*";
	Collection<ParseTree> es = AntlrUtil.xpathSubTrees(tree,xpath,parser);
	String value = "";
	for(ParseTree e: es) {
	    value = AntlrUtil.xpathSubTreeText(e,"*/dataName",parser);
	}
	assertThat(value,is("XXXX"));		
    }

    @Test
    public void testApp3() throws Exception 
    {
	InputStream is = toInputStream("01 XXXX PIC 9(10). ");
	Cobol85Parser parser = App.createParser(is);
	ParseTree tree = parser.dataDescriptionEntry();
	String xpath = "//dataDescriptionEntry/*";
	Collection<ParseTree> es = AntlrUtil.xpathSubTrees(tree,xpath,parser);
	String value = "";
	for(ParseTree e: es) {
	    value = AntlrUtil.xpathSubTreeText(e,"*//pictureString",parser);
	}
	assertThat(value,is("9(10)"));	
    }


    @Test
    public void testApp4() throws Exception 
    {
	InputStream is = toInputStream("01 XXXX PIC 9(10). ");
	Cobol85Parser parser = App.createParser(is);
	ParseTree tree = parser.dataDescriptionEntry();
	String xpath = "//dataDescriptionEntry/*";
	Collection<ParseTree> es = AntlrUtil.xpathSubTrees(tree,xpath,parser);
	String value = "";
	for(ParseTree e: es) {
	    value = AntlrUtil.xpathSubTreePatternText(e,
						      "*//dataPictureClause",
						      "<PIC> <foo:pictureString>",
						      "foo",
						      parser);
	}
	assertThat(value,is("9(10)"));
    }


    @Test
    public void testApp5() throws Exception 
    {
	InputStream is = toInputStream("88 XXXX 10 THRU 20. ");
	Cobol85Parser parser = App.createParser(is);
	ParseTree tree = parser.dataDescriptionEntry();
	String xpath = "//dataDescriptionEntry/*";
	Collection<ParseTree> es = AntlrUtil.xpathSubTrees(tree,xpath,parser);
	String value = "";
	for(ParseTree e: es) {
	    value = AntlrUtil.xpathSubTreeText(e,"*/LEVEL_NUMBER_88",parser);
	}
	assertThat(value,is("88"));	
    }


    
    
}

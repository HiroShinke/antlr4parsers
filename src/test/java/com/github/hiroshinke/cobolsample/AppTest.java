

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
import java.util.List;

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
	String value = AntlrUtil.xpathSubTreesCont
	    (parser,tree,"//dataDescriptionEntry/*",
	     (entries) -> {
		 for(ParseTree e: entries){
		     return AntlrUtil.xpathSubTreeText(parser,e,
						       "*/INTEGERLITERAL");
		 }
		 return null;
	     });
	assertThat(value,is("01"));			     
    }

    @Test
    public void testApp2() throws Exception 
    {
	InputStream is = toInputStream("01 XXXX PIC 9(10). ");
	Cobol85Parser parser = App.createParser(is);
	ParseTree tree = parser.dataDescriptionEntry();
	String xpath = "//dataDescriptionEntry/*";
	Collection<ParseTree> es = AntlrUtil.xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = AntlrUtil.xpathSubTreeText(parser,e,"*/dataName");
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
	Collection<ParseTree> es = AntlrUtil.xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = AntlrUtil.xpathSubTreeText(parser,e,"*//pictureString");
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
	Collection<ParseTree> es = AntlrUtil.xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = AntlrUtil.xpathSubTreePatternText(parser,
						      e,
						      "*//dataPictureClause",
						      "<PIC> <foo:pictureString>",
						      "foo");
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
	Collection<ParseTree> es = AntlrUtil.xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = AntlrUtil.xpathSubTreeText(parser,e,"*/LEVEL_NUMBER_88");
	}
	assertThat(value,is("88"));	
    }

    @Test
    public void testApp6() throws Exception 
    {
	InputStream is = toInputStream("01 XXXX PIC 9(10). ");
	Cobol85Parser parser = App.createParser(is);
	ParseTree tree = parser.dataDescriptionEntry();
	String xpath = "//dataDescriptionEntry/*";
	Collection<ParseTree> es = AntlrUtil.xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = AntlrUtil.xpathSubTreeText
		(parser,
		 e,
		 List.of("*/INTEGERLITERAL","*/LEVEL_NUMBER_88")
		 );
	}
	assertThat(value,is("01"));	
    }

}

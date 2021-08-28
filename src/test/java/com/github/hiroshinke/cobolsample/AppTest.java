

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
import java.util.stream.Collectors;

import org.antlr.v4.runtime.tree.xpath.XPath;
import org.antlr.v4.runtime.tree.ParseTree;

import static com.github.hiroshinke.cobolsample.ParserCommon.pattern;
import static com.github.hiroshinke.antlr4.AntlrUtil.*;

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
	String value = xpathSubTreesCont
	    (parser,tree,"//dataDescriptionEntry/*",
	     (entries) -> {
		 for(ParseTree e: entries){
		     return xpathSubTreeText(parser,e,
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
	Collection<ParseTree> es = xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = xpathSubTreeText(parser,e,"*/dataName");
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
	Collection<ParseTree> es = xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = xpathSubTreeText(parser,e,"*//pictureString");
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
	Collection<ParseTree> es = xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = xpathSubTreePatternText(parser,
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
	Collection<ParseTree> es = xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = xpathSubTreeText(parser,e,"*/LEVEL_NUMBER_88");
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
	Collection<ParseTree> es = xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = xpathSubTreeText
		(parser,
		 e,
		 List.of("*/INTEGERLITERAL","*/LEVEL_NUMBER_88")
		 );
	}
	assertThat(value,is("01"));	
    }

    @Test
    public void testApp7() throws Exception 
    {
	java.util.regex.Pattern pat = pattern;
	java.util.regex.Matcher m   = pat.matcher("012345-  \"xxxxx");
	assertThat(m.find(),is(true));
	assertThat(m.end(),is(10));	

	m   = pat.matcher("012345-  \'xxxxx");
	assertThat(m.find(),is(true));
	assertThat(m.end(),is(10));	
    }

    @Test
    public void testFileControl() throws Exception 
    {
	InputStream is = toInputStream("SELECT FILE01 ASSIGN TO DD01. ");
	Cobol85Parser parser = App.createParser(is);
	ParseTree tree = parser.fileControlEntry();
	String xpath = "//fileName";
	Collection<ParseTree> es = xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = e.getText();
	}
	assertThat(value,is("FILE01"));	

	xpath = "//assignmentName";
	es = xpathSubTrees(parser,tree,xpath);
	for(ParseTree e: es) {
	    value = e.getText();
	}
	assertThat(value,is("DD01"));	
    }

    @Test
    public void testFileDescription() throws Exception 
    {
	InputStream is = toInputStream("FD FILE01. \n"+
				       "   BLOCK CONTAINS 10 TO 20 RECORDS. \n"+
				       "01 IN-REC. \n"+
				       "   03 XXX PIC X(10). \n");
	Cobol85Parser parser = App.createParser(is);
	ParseTree tree = parser.fileDescriptionEntry();
	String xpath = "*/fileName";
	Collection<ParseTree> es = xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = e.getText();
	}
	assertThat(value,is("FILE01"));	

	xpath = "//dataDescriptionEntry/*/dataName";
	es = xpathSubTrees(parser,tree,xpath);
	List<String> ret = es.stream()
	    .map(ParseTree::getText)
	    .collect(Collectors.toList());
	assertThat(ret,is(List.of("IN-REC","XXX")));	
    }

    @Test
    public void testReadStatement() throws Exception 
    {
	InputStream is = toInputStream("READ F01 INTO IN-REC. ");
	Cobol85Parser parser = App.createParser(is);
	ParseTree tree = parser.readStatement();
	String xpath = "*/fileName";
	Collection<ParseTree> es = xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = e.getText();
	}
	assertThat(value,is("F01"));	

	xpath = "*/readInto/identifier";
	es = xpathSubTrees(parser,tree,xpath);
	for(ParseTree e: es) {
	    value = e.getText();
	}
	assertThat(value,is("IN-REC"));	
    }


    
    @Test
    public void testWriteStatement() throws Exception 
    {
	InputStream is = toInputStream("WRITE OUT-REC FROM WK-REC. ");
	Cobol85Parser parser = App.createParser(is);
	ParseTree tree = parser.writeStatement();
	String xpath = "*/recordName";
	Collection<ParseTree> es = xpathSubTrees(parser,tree,xpath);
	String value = "";
	for(ParseTree e: es) {
	    value = e.getText();
	}
	assertThat(value,is("OUT-REC"));	

	xpath = "//writeFromPhrase/identifier";
	es = xpathSubTrees(parser,tree,xpath);
	for(ParseTree e: es) {
	    value = e.getText();
	}
	assertThat(value,is("WK-REC"));	
    }


    @Test
    public void testOpenStatement() throws Exception 
    {
	InputStream is = toInputStream("OPEN INPUT X Y OUTPUT Z W. ");
	Cobol85Parser parser = App.createParser(is);
	ParseTree tree = parser.openStatement();
	String xpath = "//openInputStatement//fileName";
	Collection<ParseTree> es = xpathSubTrees(parser,tree,xpath);
	List<String> ret = es.stream()
	    .map( ParseTree::getText )
	    .collect(Collectors.toList());
	assertThat(ret,is(List.of("X","Y")));

	xpath = "//openOutputStatement//fileName";
	es = xpathSubTrees(parser,tree,xpath);
	ret = es.stream()
	    .map( ParseTree::getText )
	    .collect(Collectors.toList());
	assertThat(ret,is(List.of("Z","W")));
    }

    


    
}

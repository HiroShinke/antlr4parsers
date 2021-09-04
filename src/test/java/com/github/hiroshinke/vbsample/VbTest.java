



package com.github.hiroshinke.vbsample;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.Rule;
import org.junit.rules.TemporaryFolder;
import static org.hamcrest.Matchers.is;
import java.io.IOException;
import java.io.InputStream;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.io.File;
import org.junit.contrib.java.lang.system.SystemOutRule;

import java.util.Collection;
import java.util.stream.Collectors;
import java.util.List;

import org.apache.commons.io.FileUtils;
import com.github.hiroshinke.vbsample.App;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import static com.github.hiroshinke.antlr4.AntlrUtil.*;

/**
 * Unit test for simple App.
 */
public class VbTest
{
    @Rule
    public TemporaryFolder tempFolder = new TemporaryFolder();

    @Rule
    public final SystemOutRule systemOutRule = new SystemOutRule().enableLog();

    @Test
    public void explicitCallStmt1() throws Exception 
    {
	String src = "Call SubExample1\n";
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("ProcedureCall,,SubExample1\n"));
    }

    @Test
    public void explicitCallStmt2() throws Exception 
    {
	String src = "Call SubExample1\n";
	//App.printTree(new ANTLRInputStream(src));	
	VisualBasic6Parser parser = App.createParser(new ANTLRInputStream(src));
	ParseTree tree = parser.blockStmt();
	String name = xpathSubTreeText
	    (parser,
	     tree,
	     "*/explicitCallStmt/eCS_ProcedureCall/ambiguousIdentifier");
	assertThat(name,is("SubExample1"));
    }

    @Test
    public void explicitCallStmt3() throws Exception 
    {
	String src = "Call SubExample1(\"some arg\")\n";
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("ProcedureCall,,SubExample1\n"));
    }

    @Test
    public void explicitCallStmt4() throws Exception 
    {
	String src = "Call SubExample1(\"some arg\")\n";
	//App.printTree(new ANTLRInputStream(src));	
	VisualBasic6Parser parser = App.createParser(new ANTLRInputStream(src));
	ParseTree tree = parser.blockStmt();
	String name = xpathSubTreeText
	    (parser,
	     tree,
	     "*/explicitCallStmt/eCS_ProcedureCall/ambiguousIdentifier");
	assertThat(name,is("SubExample1"));
    }

    
    @Test
    public void implicitCallStmt_InBlock1() throws Exception 
    {
	String src = "SubExample1\n";
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("ProcedureCall,,SubExample1\n"));
    }

    @Test
    public void implicitCallStmt_InBlock2() throws Exception 
    {
	String src = "SubExample1\n";
	//App.printTree(new ANTLRInputStream(src));	
	VisualBasic6Parser parser = App.createParser(new ANTLRInputStream(src));
	ParseTree tree = parser.blockStmt();
	String name = xpathSubTreeText
	    (parser,
	     tree,
	     "*/implicitCallStmt_InBlock/iCS_B_ProcedureCall/certainIdentifier");
	assertThat(name,is("SubExample1"));
    }


    @Test
    public void implicitCallStmt_InBlock3() throws Exception 
    {
	String src = "SubExample1 \"some arg\"\n";
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("ProcedureCall,,SubExample1\n"));
    }

    @Test
    public void implicitCallStmt_InBlock4() throws Exception 
    {
	String src = "SubExample1 \"some arg\"\n";
	//App.printTree(new ANTLRInputStream(src));	
	VisualBasic6Parser parser = App.createParser(new ANTLRInputStream(src));
	ParseTree tree = parser.blockStmt();
	String name = xpathSubTreeText
	    (parser,
	     tree,
	     "*/implicitCallStmt_InBlock/iCS_B_ProcedureCall/certainIdentifier");
	assertThat(name,is("SubExample1"));
    }

    @Test
    public void nestedCall1() throws Exception 
    {
	String src = "x = f(g(1,2,3),4) \n";
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("ProcedureCall,,x\n" +
		      "ProcedureCall,,f\n" +	
		      "ProcedureCall,,g\n"));
    }

    @Test
    public void nestedCall2() throws Exception 
    {
	String src = "x = f(g(1,2,3),4) \n";
	//App.printTree(new ANTLRInputStream(src));		
	VisualBasic6Parser parser = App.createParser(new ANTLRInputStream(src));
	ParseTree tree = parser.startRule();
	List<String> name = xpathSubTrees
	    (parser,
	     tree,
	     "//implicitCallStmt_InStmt//iCS_S_ProcedureOrArrayCall/ambiguousIdentifier").
	    stream().
	    map( n -> n.getText() ).collect(Collectors.toList());
	
	assertThat(name,is(List.of("f",
				   "g")));
    }


    @Test
    public void nestedCall3() throws Exception 
    {
	String src = "x = f(g(1,2,3),4) \n";
	VisualBasic6Parser parser = App.createParser(new ANTLRInputStream(src));
	ParseTree tree = parser.startRule();
	List<String> name = xpathSubTrees
	    (parser,
	     tree,
	     "//implicitCallStmt_InStmt//iCS_S_VariableOrProcedureCall/ambiguousIdentifier").
	    stream().
	    map( n -> n.getText() ).collect(Collectors.toList());
	
	assertThat(name,is(List.of("x")));
    }

    
    @Test
    public void yyyyy0() throws Exception 
    {
	String src = 
	    "Private Sub cmdClear_Click()\n" +
	    "txtHello.Text = \"\"\n" +
	    "End Sub\n";
	//App.printTree(new ANTLRInputStream(src));
	VisualBasic6Parser parser = App.createParser(new ANTLRInputStream(src));
	ParseTree tree = parser.startRule();
	List<String> name = xpathSubTrees
	    (parser,
	     tree,
	     "//implicitCallStmt_InStmt//iCS_S_VariableOrProcedureCall/ambiguousIdentifier").
	    stream().
	    map( n -> n.getText()).collect(Collectors.toList());
	
	assertThat(name,is(List.of("txtHello",
				   "Text")));
    }

    @Test
    public void yyyyy() throws Exception 
    {
	String src = 
	    "Private Sub cmdClear_Click()\n" +
	    "txtHello.Text = \"\"\n" +
	    "End Sub\n";
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("ProcedureCall,,txtHello\n" +
		      "ProcedureCall,,Text\n"));
    }

    
}

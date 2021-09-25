



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
public class Vb2Test
{
    @Rule
    public TemporaryFolder tempFolder = new TemporaryFolder();

    @Rule
    public final SystemOutRule systemOutRule = new SystemOutRule().enableLog();

    @Test
    public void implicitCallStmt_S1() throws Exception 
    {
	String src = 
	    "Private Sub cmdClear_Click()\n" +
	    "Dim x As Integar\n" +
	    "txtHello.Text = \"\"\n" +
	    "End Sub\n";
	App.printTree(new ANTLRInputStream(src));
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("subStmt,cmdClear_Click,Private\n"+
		      "variableStmt,,,x\n"+
		      "letStmt,,,txtHello.Text\n"
		      ));
					
    }


    @Test
    public void explicitCallStmt1() throws Exception 
    {
	String src = 
	    "Private Sub cmdClear_Click()\n" +
	    "Call subExample1\n" +
	    "End Sub\n";
	App.printTree(new ANTLRInputStream(src));
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("subStmt,cmdClear_Click,Private\n"+
		      "explicitCallStmt,,,subExample1\n"
		      ));
					
    }

    
    @Test
    public void implicitCallStmt_S2() throws Exception 
    {
	String src = 
	    "Private Sub cmdClear_Click()\n" +
	    "txtHello.Text = funcExample1()\n" +
	    "End Sub\n";
	App.printTree(new ANTLRInputStream(src));
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("subStmt,cmdClear_Click,Private\n"+
		      "letStmt,,,txtHello.Text\n"+
		      "valueStmt,,,funcExample1()\n"
		      ));
					
    }

    @Test
    public void implicitCallStmt_S3() throws Exception 
    {
	String src = 
	    "Private Sub cmdClear_Click()\n" +
	    "txtHello.Text = funcExample1\n" +
	    "End Sub\n";
	App.printTree(new ANTLRInputStream(src));
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("subStmt,cmdClear_Click,Private\n"+
		      "letStmt,,,txtHello.Text\n"+
		      "valueStmt,,,funcExample1\n"
		      ));
					
    }

    @Test
    public void implicitCallStmt_S4() throws Exception 
    {
	String src = 
	    "Private Sub cmdClear_Click()\n" +
	    "txtHello.Text = funcExample1(funcExample2())\n" +
	    "End Sub\n";
	App.printTree(new ANTLRInputStream(src));
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("subStmt,cmdClear_Click,Private\n"+
		      "letStmt,,,txtHello.Text\n"+
		      "valueStmt,,,funcExample1(funcExample2())\n"+
		      "valueStmt,,,funcExample2()\n"		      
		      ));
					
    }


    @Test
    public void implicitCallStmt_S5() throws Exception 
    {
	String src = 
	    "Private Sub cmdClear_Click()\n" +
	    "txtHello.Text = funcExample1(funcExample2)\n" +
	    "End Sub\n";
	App.printTree(new ANTLRInputStream(src));
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("subStmt,cmdClear_Click,Private\n"+
		      "letStmt,,,txtHello.Text\n"+
		      "valueStmt,,,funcExample1(funcExample2)\n"+
		      "valueStmt,,,funcExample2\n"		      
		      ));
					
    }
    
    @Test
    public void declareStmt1() throws Exception 
    {
	String src = 
	    "Dim x As Integar\n" +
	    "Private Sub cmdClear_Click()\n" +
	    "txtHello.Text = \"\"\n" +
	    "End Sub\n";
	App.printTree(new ANTLRInputStream(src));
	App.printInfo("",new ANTLRInputStream(src));
	assertThat(systemOutRule.getLog(),
		   is("variableStmt,,,x\n"+
		      "subStmt,cmdClear_Click,Private\n"+
		      "letStmt,,,txtHello.Text\n"
		      ));
					
    }


    
}

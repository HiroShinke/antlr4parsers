

package com.github.hiroshinke.antlrsample;

import static org.junit.Assert.*;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Unit test for simple App.
 */
public class GrunUtil2Test
{

    ListExpr l(Expr... exps){
	ListExpr l = new ListExpr();
	for (Expr e: exps){
	    l.addElement(e);
	}
	return l;
    }

    AtomExpr a(String str){
	return new AtomExpr(str);
    }

    Expr readExpr(String str){
	GrunReader g = new GrunReader(str);
	ParseResult r = g.readExpr(0);
	if( r != null ){
	    return r.e;
	} else {
	    return null;
	}
    }

    ArrayList<String> strList(String... args){
	ArrayList<String> buff = new ArrayList<String>();
	buff.addAll(Arrays.asList(args));
	return buff;
    }
    
    @Test
    public void testApp1()
    {
	Expr e = readExpr("(xxx (aaa 1))");
	ArrayList<String> r  = GrunUtil.findPath(e,"//xxx");
	//System.out.println( "r=" + r );
	assertTrue( r.equals(strList("xxx")) );
    }

    @Test
    public void testApp2()
    {
	Expr e = readExpr("(xxx (aaa 1))");
	ArrayList<String> r = GrunUtil.findPath(e,"//aaa");
	//System.out.println( "r=" + r );	
	assertTrue( r.equals(strList("xxx/aaa")) );
    }

    @Test
    public void testApp3()
    {
	Expr e = readExpr("(xxx (aaa 1) (aaa 2))");
	ArrayList<String> r = GrunUtil.findPath(e,"//aaa");
	//System.out.println( "r=" + r );	
	assertTrue( r.equals(strList("xxx/aaa","xxx/aaa")) );
    }

    @Test
    public void testApp4()
    {
	Expr e = readExpr("(xxx (aaa 1) (aaa 2))");
	ArrayList<String> r = GrunUtil.findPath(e,"//1");
	//System.out.println( "r=" + r );	
	assertTrue( r.equals(strList("xxx/aaa/1")) );
    }

    @Test
    public void testApp5()
    {
	Expr e = readExpr("(xxx (aaa 1) (aaa (bbb (ccc 1))))");
	ArrayList<String> r = GrunUtil.findPath(e,"//1");
	//System.out.println( "r=" + r );	
	assertTrue( r.equals(strList("xxx/aaa/1","xxx/aaa/bbb/ccc/1")) );
    }


    @Test
    public void testApp6()
    {
	Expr e = readExpr("(xxx (aaa 1) (aaa (bbb (ccc 1))))");
	ArrayList<String> r = GrunUtil.findPath(e,"//aaa/bbb/ccc/1");
	//System.out.println( "r=" + r );	
	assertTrue( r.equals(strList("xxx/aaa/bbb/ccc/1")) );
    }

    @Test
    public void testApp7()
    {
	Expr e = readExpr("(xxx (aaa 1) (aaa (bbb (ccc 1))))");
	ArrayList<String> r = GrunUtil.findPath(e,"//aaa//ccc/1");
	//System.out.println( "r=" + r );	
	assertTrue( r.equals(strList("xxx/aaa/bbb/ccc/1")) );
    }

    
    
}

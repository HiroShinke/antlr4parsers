

package com.github.hiroshinke.antlrsample;

import static org.junit.Assert.*;
import org.junit.Test;


/**
 * Unit test for simple App.
 */
public class GrunUtilTest
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

    /**
     * Rigourous Test :-)
     */
    @Test
    public void testApp1()
    {
	Expr e = readExpr("1");
	assertTrue( e.equals(a("1")) );
    }

    /**
     * Rigourous Test :-)
     */
    @Test
    public void testApp2()
    {
	Expr e = readExpr("(xxx 1)");
	ListExpr a = l( a("xxx"), a("1"));
	assertTrue( e.equals(a) );
    }

    /**
     * Rigourous Test :-)
     */
    @Test
    public void testApp3()
    {
	Expr e = readExpr("(xxx 1 2 3)");
	ListExpr a = l( a("xxx"), a("1"), a("2"), a("3"));
	assertTrue( e.equals(a) );
    }

    /**
     * Rigourous Test :-)
     */
    @Test
    public void testApp4()
    {
	Expr e = readExpr("(xxx (1) 2 3)");
	ListExpr a = l( a("xxx"), l(a("1")), a("2"), a("3"));
	assertTrue( e.equals(a) );
    }

    /**
     * Rigourous Test :-)
     */
    @Test
    public void testApp5()
    {
	Expr e = readExpr("(xxx ((1) 2) 3)");
	ListExpr a = l( a("xxx"), l(l(a("1")), a("2")), a("3"));
	assertTrue( e.equals(a) );
    }
    
    /**
     * Rigourous Test :-)
     */
    @Test
    public void testApp6()
    {
	Expr e = readExpr("(xxx ( 2 3)");
	ListExpr a = l( a("xxx"), a("("), a("2"), a("3"));
	assertTrue( e.equals(a) );
    }


    @Test
    public void testApp7()
    {
	Expr e = readExpr("(xxx ) 2 3)");
	ListExpr a = l( a("xxx"), a(")"), a("2"), a("3"));
	assertTrue( e.equals(a) );
    }

    @Test
    public void testApp8()
    {
	Expr e = readExpr(")");
	assertTrue( e == null );
    }

    @Test
    public void testApp9()
    {
	Expr e = readExpr("(");
	System.out.println(e);
	assertTrue( e == null );
    }

    



}

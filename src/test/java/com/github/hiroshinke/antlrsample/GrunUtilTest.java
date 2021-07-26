

package com.github.hiroshinke.antlrsample;

import static org.junit.Assert.*;
import org.junit.Test;


/**
 * Unit test for simple App.
 */
public class GrunUtilTest
{
    /**
     * Rigourous Test :-)
     */
    @Test
    public void testApp()
    {

	GrunUtil g = new GrunUtil("(xxx 1)");
	ParseResult r = g.readExpr(0);
	ListExpr a = new ListExpr();
	a.addElement(new AtomExpr("xxx"));
	a.addElement(new AtomExpr("1"));
	assertTrue( r.e.equals(a) );

	g = new GrunUtil("1");
	r = g.readExpr(0);
	assertTrue( r.e.equals(new AtomExpr("1")));

    }
}

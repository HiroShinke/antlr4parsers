

package com.github.hiroshinke.antlrsample;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit test for simple App.
 */
public class GrunUtilTest
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public GrunUtilTest( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( GrunUtilTest.class );
    }

    /**
     * Rigourous Test :-)
     */
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

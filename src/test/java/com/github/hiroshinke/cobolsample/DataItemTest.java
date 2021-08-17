

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

/**
 * Unit test for simple App.
 */
public class DataItemTest
{

    @Test
    public void pictX() throws Exception 
    {
	int n = DataItem.pictureCountChar("X");
	assertThat(n,is(1));			     
    }

    @Test
    public void pictXXX() throws Exception 
    {
	int n = DataItem.pictureCountChar("XXX");
	assertThat(n,is(3));
    }

    @Test
    public void pictX10() throws Exception 
    {
	int n = DataItem.pictureCountChar("X(10)");
	assertThat(n,is(10));
    }

    @Test
    public void pictX10X10() throws Exception 
    {
	int n = DataItem.pictureCountChar("X(10)X(10)");
	assertThat(n,is(20));
    }
    
    @Test
    public void pict999() throws Exception 
    {
	int n = DataItem.pictureCountChar("999");
	assertThat(n,is(3));
    }

    @Test
    public void pictS999() throws Exception 
    {
	int n = DataItem.pictureCountChar("S999.999");
	assertThat(n,is(6));
    }

    @Test
    public void pict9_10_9_3() throws Exception 
    {
	int n = DataItem.pictureCountChar("9(10).9(3)");
	assertThat(n,is(13));
    }

    @Test
    public void sizeX() throws Exception 
    {
	int n = DataItem.calculateSize("DISPLAY","X");
	assertThat(n,is(1));			     
    }

    @Test
    public void size9() throws Exception 
    {
	int n = DataItem.calculateSize("DISPLAY","9(10)");
	assertThat(n,is(10));			     
    }

    @Test
    public void size9_10() throws Exception 
    {
	int n = DataItem.calculateSize("COMP-3","9(10)");
	assertThat(n,is(6));			     
    }

    @Test
    public void size9_11() throws Exception 
    {
	int n = DataItem.calculateSize("COMP-3","9(11)");
	assertThat(n,is(6));			     
    }

    @Test
    public void sizeCOMP_4() throws Exception 
    {
	int n = DataItem.calculateSize("COMP","9(9)");
	assertThat(n,is(4));			     
    }

    @Test
    public void sizeCOMP_2() throws Exception 
    {
	int n = DataItem.calculateSize("COMP","9(4)");
	assertThat(n,is(2));			     
    }
    

}

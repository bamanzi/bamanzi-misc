/***************************************************************************
                          main.cpp  -  description                              
                             -------------------                                         
    begin                : Tue Aug 17 11:30:38 CEST 1999

    copyright            : (C) 1999 by Balázs Terényi
    email                : terenyi@freemail.c3.hu

    modified by          : Mathieu Villegas
    email                : mathieu.villegas@immervision.net
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   * 
 *                                                                         *
 ***************************************************************************/

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


char* RESPONSE_END="|END|";

void strtolower(char* str)
{
   int i=0;
   while (str[i] != '\0')
   {
      str[i]=tolower(str[i]);
      i++;
   }
}

void simplify(char* ret,char* data)
{
    char** point;
    char* substring;

    point=&data;
    while ((substring=strsep(point,"\n")) != 0)
    {
	strncpy(ret+strlen(ret),substring,strlen(substring));
    }
}

char* const getHTMLTitle(FILE* f)
{
   int len,kezd;
   char* ret=0;
   char buff[500];
   char buff_original[500];
   char databuff[2000];
   char databuff_lower[2000];
   bool found=false;
   bool finish=false;

   while ((fgets(buff_original,500,f) != 0) && (!finish))
   {
      strcpy(buff,buff_original);
      strtolower(buff);
      if (strstr(buff,"<title>") != 0)
      {
         strcpy(databuff,buff_original);
         found=true;
      }
      if ((strstr(buff,"<title>") == 0) && found)
      {
	 if (strlen(databuff) + strlen(buff_original) < 2000)
	 {
             strcat(databuff,buff_original);
	 }
	 else
	 {
	     return 0;
	 }
      }
      if (strstr(buff,"</title>") != 0)
      {
         strcpy(databuff_lower,databuff);
         strtolower(databuff_lower);

         len=strstr(databuff_lower,"</title>") - (strstr(databuff_lower,"<title>")+7);
	 if (len > 0)
	 {
             ret=(char*)malloc(len+1);
	     ret[0]='\0';
             kezd=(strstr(databuff_lower,"<title>")+7) - databuff_lower;
	     strncpy(databuff_lower,databuff+kezd,len);
    	     databuff_lower[len]='\0';
    	     databuff_lower[len+1]='\0';
	     simplify(ret,databuff_lower);
	     finish=true;
          }
	  else
	  {
	      finish=true;
	  }
      }
   }

   return ret;
}

void info()
{
  printf("KADEhtml 0.1 (c) 1999 Balázs Terényi <terenyi@freemail.c3.hu>\nModified by Mathieu Villegas from the original\nHTML title extraction plugin for the KDiskCat project\n");
}


int main(int argc, char** argv)
{
  char buff[1000];
  char* fname=buff;
  char* data;
  FILE *f;
  
  if((!strcmp(argv[argc - 1],"--help"))||(argc == 1))
    {
      info();
      exit(0);
    }
  
  if ((f=fopen(argv[1],"r")) != 0)
    {
      data=getHTMLTitle(f);
      if (data != 0)
	{
	  printf("%s\n", data);
	  fflush(stdout);
	  free(data);
	}
      fclose(f);
    }
}


#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#  benford.py
#  
#  Copyright 2014 Leandro <Leandro@leandrowar>
#  

import math as m
import pylab as py
import csv

###################
#Funções do código#
###################

def lei_benford():
	benf = [m.log10(1+1/float(i))*100.0 for i in xrange(1,10)]
	return benf

def calcula_digito(dados):
   fdigito = [str(dado)[:1] for dado in dados]
   primeiro_d = [fdigito.count(str(i))/float(len(dados))*100 for i in xrange(1, 10)]
   return primeiro_d

def pearson_correl(x,y): #fonte - http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient
   sizex = len(x)
   sizey = len(y)
   if sizex != sizey: return 0
   if sizey == 0: return 0
   sizen = float(sizex)
   media_x = sum(x)/sizen
   media_y = sum(y)/sizen
   desvio_x = m.sqrt(sum([(a-media_x)*(a-media_x) for a in x])/(sizen-1) )
   desvio_y = m.sqrt(sum([(a-media_y)*(a-media_y) for a in y])/(sizen-1) )
   normx = [(a-media_x)/desvio_x for a in x]
   normy = [(a-media_y)/desvio_y for a in y]
   return sum([normx[i]*normy[i] for i in range(sizex)])/(sizen-1)

def plot_benford(benford, data_ibov, dados_nome):
   xaxis = py.arange(1, 10)
   py.plot(xaxis, benford, linewidth=1.0)
   py.plot(xaxis, data_ibov, linewidth=1.0)
   py.xlabel('Primeiro Digito')
   py.ylabel('Percentual')
   py.title("Benford: %s (Correlacao:  %.2f)" % (dados_nome, pearson_correl(benford, data_ibov)))
   py.legend((dados_nome, "Lei de Benford Aplicada"))
   return py.show()
   
def ajusta(valor):
	v = float(valor.replace(",","."))
	return v

###############################################################
#Calculo do das variáveis do gráfico Acoes Ibovespa Fechamento#
###############################################################

benford_ibov = open("benford_ibov.csv", "r")
benford_ibov = csv.reader(benford_ibov, delimiter=";")
ibov_formated = benford_ibov.next()
close_prices = [ ajusta(row[ibov_formated.index("CLOSE")]) for row in benford_ibov ]

plot_benford(calcula_digito(close_prices), lei_benford(), "Acoes Ibovespa Fechamento - 08.05.2014")

#########################################################################
#Calculo do das variáveis do gráfico Acoes Ibovespa Variacao em 12 meses#
#########################################################################

benford_ibov = open("benford_ibov.csv", "r")
benford_ibov = csv.reader(benford_ibov, delimiter=";")
ibov_formated = benford_ibov.next()
year_variation = [ ajusta(row[ibov_formated.index("VAR_12M")]) for row in benford_ibov ]

plot_benford(calcula_digito(year_variation), lei_benford(), "Acoes Ibovespa Variacao em 12 meses - 08.05.2014")

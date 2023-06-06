#pragma once

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include "byte.h"

uint32_t mask (uint32_t a, uint32_t n);
void afficher_tab(uint32_t tab[4]);
bool eq_tableau (uint32_t tab[],int b, uint32_t verif[],int a);
void renvoi(bool a);
uint32_t take_byte (uint32_t word, int x);
uint32_t setByte (uint32_t word,int x, uint32_t b);
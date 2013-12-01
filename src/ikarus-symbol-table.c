/*
 * Ikarus Scheme -- A compiler for R6RS Scheme.
 * Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
 * Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
 *
 * This program is free software:  you can redistribute it and/or modify
 * it under  the terms of  the GNU General  Public License version  3 as
 * published by the Free Software Foundation.
 *
 * This program is  distributed in the hope that it  will be useful, but
 * WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
 * MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
 * General Public License for more details.
 *
 * You should  have received  a copy of  the GNU General  Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include "internals.h"

#undef NUM_OF_BUCKETS
#define NUM_OF_BUCKETS		IK_CHUNK_SIZE /* power of 2 */


static ikptr
make_symbol_table (ikpcb* pcb)
/* Build and return a new hash table to be used as symbol table for both
   common  symbols and  gensyms.   "Symbol table"  here  means a  Scheme
   vector of  buckets, in which  a bucket is  a proper list  of symbols;
   empty bucket slots are initialised to the fixnum zero.

   The vector is allocated outside  of the memory scanned by the garbage
   collector.  Later  some pages in the  vector may be  registered to be
   scanned. */
{
  int   size = IK_ALIGN_TO_NEXT_PAGE(disp_vector_data + NUM_OF_BUCKETS * wordsize);
  ikptr st   = ik_mmap_ptr(size, 0, pcb) | vector_tag;
  memset((char*)(long)st-vector_tag, '\0', size);
  IK_REF(st, off_vector_length) = IK_FIX(NUM_OF_BUCKETS);
  return st;
}
ikptr
ikrt_get_symbol_table (ikpcb* pcb)
/* The symbol  table is created by  C language code,  but, after loading
   the  boot  image,  it is  retrieved  by  Scheme  code to  be  handled
   there. */
{
  ikptr st = pcb->symbol_table;
  pcb->symbol_table = IK_FALSE_OBJECT;
  if (st == IK_FALSE_OBJECT)
    ik_abort("attempt to access dead symbol table");
  return st;
}


static long
compute_string_hash (ikptr str)
/* one-at-a-time from http://burtleburtle.net/bob/hash/doobs.html */
{
  long  len  = IK_UNFIX(IK_REF(str, off_string_length));
  int*  data = (int*)(str + off_string_data);
  int   h    = len;
  int*  last = data + len;
  /* one-at-a-time */
  while (data < last) {
    int c = (*data >> 8);
    h = h + c;
    h = h + (h << 10);
    h = h ^ (h >> 6);
    data++;
  }
  h = h + (h << 3);
  h = h ^ (h >> 11);
  h = h + (h << 15);
  return (h >= 0) ? h : (1 - h);
}
ikptr
ikrt_string_hash (ikptr str)
{
  return (ikptr)(compute_string_hash(str) & (~ fx_mask));
}
static int
strings_eqp (ikptr str1, ikptr str2)
{
  ikptr len = IK_REF(str1, off_string_length);
  if (len == IK_REF(str2, off_string_length)) {
    return (0 == memcmp((char*)(long)str1+off_string_data,
			(char*)(long)str2+off_string_data,
			IK_UNFIX(len) * IK_STRING_CHAR_SIZE));
  } else
    return 0;
}
ikptr
ikrt_bytevector_hash (ikptr bv)
{
  long		len  = IK_BYTEVECTOR_LENGTH(bv);
  uint8_t *	data = IK_BYTEVECTOR_DATA_UINT8P(bv);
  int		h    = len;
  uint8_t *	last = data + len;
  /* one-at-a-time */
  while (data < last) {
    int c = (*data >> 8);
    h = h + c;
    h = h + (h << 10);
    h = h ^ (h >> 6);
    data++;
  }
  h = h + (h << 3);
  h = h ^ (h >> 11);
  h = h + (h << 15);
  return (((h >= 0) ? h : (1 - h)) & (~ fx_mask));
}


static ikptr
iku_make_symbol (ikptr s_pretty_string, ikptr s_unique_string, ikpcb* pcb)
{
  ikptr s_sym = ik_unsafe_alloc(pcb, symbol_record_size) | record_tag;
  IK_REF(s_sym, -record_tag)               = symbol_tag;
  IK_REF(s_sym, off_symbol_record_string)  = s_pretty_string;
  IK_REF(s_sym, off_symbol_record_ustring) = s_unique_string;
  IK_REF(s_sym, off_symbol_record_value)   = IK_UNBOUND_OBJECT;
  IK_REF(s_sym, off_symbol_record_proc)    = s_pretty_string;
  IK_REF(s_sym, off_symbol_record_plist)   = IK_NULL_OBJECT;
  return s_sym;
}
static ikptr
intern_string (ikptr s_unique_string, ikptr s_symbol_table, ikpcb* pcb)
/* Notice that all the memory allocations here are UNsafe. */
{
  int   hash_value    = compute_string_hash(s_unique_string);
  int   bucket_index  = hash_value & (IK_VECTOR_LENGTH(s_symbol_table) - 1);
  ikptr s_bucket_list = IK_ITEM(s_symbol_table, bucket_index);
  { /* If a symbol having S_UNIQUE_STRING is already interned: return it
       to the caller. */
    ikptr s_list_iterator = s_bucket_list;
    while (s_list_iterator && IK_NULL_OBJECT != s_list_iterator) {
      ikptr s_sym     = IK_CAR(s_list_iterator);
      ikptr s_sym_str = IK_REF(s_sym, off_symbol_record_string);
      if (strings_eqp(s_sym_str, s_unique_string)) {
	return s_sym;
      }
      s_list_iterator = IK_CDR(s_list_iterator);
    }
  }
  /* Allocate a new  pointer object and register it  in the symbol table
     by prepending it to the bucket list. */
  ikptr s_sym  = iku_make_symbol(s_unique_string, IK_FALSE_OBJECT, pcb);
  ikptr s_pair = IKU_PAIR_ALLOC(pcb);
  IK_CAR(s_pair) = s_sym;
  IK_CDR(s_pair) = s_bucket_list;
  IK_ITEM(s_symbol_table, bucket_index) = s_pair;
  { /* Mark the  page containing  the bucket slot  to be scanned  by the
       garbage collector. */
    ik_ulong bucket_slot_pointer = s_symbol_table + off_vector_data + bucket_index * wordsize;
    ((int*)(long)pcb->dirty_vector)[IK_PAGE_INDEX(bucket_slot_pointer)] = -1;
  }
  return s_sym;
}
static ikptr
intern_unique_string (ikptr s_pretty_string, ikptr s_unique_string, ikptr s_symbol_table, ikpcb* pcb)
/* Intern a  symbol object, having  S_PRETTY_STRING and S_UNIQUE_STRING,
   in S_SYMBOL_TABLE which can be  either the common symbol table or the
   gensyms table.

   If a symbol object having S_UNIQUE_STRING is already interned: return
   it.  Else: allocate a new symbol object, intern it, return it.

   Notice that all the memory allocations here are UNsafe. */
{
  int   hash_value    = compute_string_hash(s_unique_string);
  int   bucket_index  = hash_value & (IK_VECTOR_LENGTH(s_symbol_table) - 1);
  ikptr s_bucket_list = IK_ITEM(s_symbol_table, bucket_index);
  { /* If a  symbol having  S_UNIQUE_STRING is already  interned, return
       it. */
    ikptr s_list_iterator = s_bucket_list;
    while (s_list_iterator && IK_NULL_OBJECT != s_list_iterator) {
      ikptr s_sym      = IK_CAR(s_list_iterator);
      ikptr s_sym_ustr = IK_REF(s_sym, off_symbol_record_ustring);
      if (strings_eqp(s_sym_ustr, s_unique_string)) {
	return s_sym;
      }
      s_list_iterator = IK_CDR(s_list_iterator);
    }
  }
  /* Allocate a  new symbol  object and  add it to  the symbol  table by
     prepending it to the bucket list. */
  ikptr s_sym  = iku_make_symbol(s_pretty_string, s_unique_string, pcb);
  ikptr s_pair = IKU_PAIR_ALLOC(pcb);
  IK_CAR(s_pair) = s_sym;
  IK_CDR(s_pair) = s_bucket_list;
  IK_ITEM(s_symbol_table, bucket_index) = s_pair;
  { /* Mark the  page containing  the bucket slot  to be scanned  by the
       garbage collector. */
    ik_ulong bucket_slot_pointer = s_symbol_table + off_vector_data + bucket_index * wordsize;
    ((int*)(long)pcb->dirty_vector)[IK_PAGE_INDEX(bucket_slot_pointer)] = -1;
  }
  return s_sym;
}
ikptr
ikrt_intern_gensym (ikptr s_sym, ikpcb* pcb)
/* Given a  symbol object try to intern  it in the table  of gensyms; if
   successful  return the  true object,  else return  the  false object.
   Successful interning  means that  the unique string  in S_SYM  is not
   already in the table.

   Interning a gensym  cannot fail; this function is  meant to be called
   in a  loop with a  newly generated unique  string stored in  S_SYM at
   each iteration until the interning succeeds. */
{
  ikptr s_gensym_table = pcb->gensym_table;
  if (0 == s_gensym_table) {
    pcb->gensym_table = s_gensym_table = make_symbol_table(pcb);
  }
  ikptr s_unique_string = IK_REF(s_sym, off_symbol_record_ustring);
  int   hash_value      = compute_string_hash(s_unique_string);
  int   bucket_index    = hash_value & (IK_VECTOR_LENGTH(s_gensym_table) - 1);
  ikptr s_bucket_list   = IK_ITEM(s_gensym_table, bucket_index);
  { /* If a  symbol having  S_UNIQUE_STRING is already  interned, return
       false. */
    ikptr s_list_iterator = s_bucket_list;
    while (s_list_iterator && IK_NULL_OBJECT != s_list_iterator) {
      ikptr s_sym      = IK_CAR(s_list_iterator);
      ikptr s_sym_ustr = IK_REF(s_sym, off_symbol_record_ustring);
      if (strings_eqp(s_sym_ustr, s_unique_string)) {
	return IK_FALSE_OBJECT;
      }
      s_list_iterator = IK_CDR(s_list_iterator);
    }
  }
  /* Allocate a  new symbol  object and  add it to  the symbol  table by
     prepending it to the bucket list. */
  ikptr s_pair = IKU_PAIR_ALLOC(pcb);
  IK_CAR(s_pair) = s_sym;
  IK_CDR(s_pair) = s_bucket_list;
  IK_ITEM(s_gensym_table, bucket_index) = s_pair;
  { /* Mark the  page containing  the bucket slot  to be scanned  by the
       garbage collector. */
    ik_ulong bucket_slot_pointer = s_gensym_table + off_vector_data + bucket_index * wordsize;
    ((int*)(long)pcb->dirty_vector)[IK_PAGE_INDEX(bucket_slot_pointer)] = -1;
  }
  return IK_TRUE_OBJECT;
}
ikptr
ikrt_unintern_gensym (ikptr s_sym, ikpcb* pcb)
/* Remove S_SYM  from the  hash table of  interned gensyms;  return true
   object if  the table exists and  S_SYM is present,  else return false
   object. */
{
  fprintf(stderr, "removing gensym\n");
  ikptr gensym_table = pcb->gensym_table;
  if (0 == gensym_table) {
    /* no symbol table */
    return IK_FALSE_OBJECT;
  }
  ikptr s_unique_string = IK_REF(s_sym, off_symbol_record_ustring);
  if (IK_TAGOF(s_unique_string) != string_tag) {
    return IK_FALSE_OBJECT;
  }
  int   hash_value    = compute_string_hash(s_unique_string);
  int   bucket_index  = hash_value & (IK_VECTOR_LENGTH(gensym_table) - 1);
#if 0
  /* This  is the  original  Ikarus  code.  (Marco  Maggi;  Sun Mar  11,
     2012) */
  ikptr bucket_list_pointer = (ikptr)(gensym_table + off_vector_data + bucket_index * wordsize);
  ikptr s_bucket_list       = IK_REF(bucket_list_pointer, 0);
  while (s_bucket_list) {
    if (IK_CAR(s_bucket_list) == s_sym) {
      /* Found it.   Remove the unique  string from the  gensym.  Remove
	 the containing pair from the bucket list. */
      IK_REF(s_sym, off_symbol_record_ustring) = IK_TRUE_OBJECT;
      IK_REF(bucket_list_pointer, 0) = IK_CDR(s_bucket_list);
      return IK_TRUE_OBJECT;
    } else {
      bucket_list_pointer = (ikptr)(s_bucket_list + off_cdr);
      s_bucket_list       = IK_REF(bucket_list_pointer, 0);
    }
  }
#else
  /* This is  the modified  code to make  things more  readable.  (Marco
     Maggi; Sun Mar 11, 2012) */
  ikptr * bucket_list_pointer = (ikptr *)(gensym_table + off_vector_data + bucket_index * wordsize);
  ikptr   s_bucket_list       = *bucket_list_pointer;
  while (s_bucket_list) {
    if (IK_CAR(s_bucket_list) == s_sym) {
      /* Found it.   Remove the unique  string from the  gensym.  Remove
	 the containing pair from the bucket list. */
      IK_REF(s_sym, off_symbol_record_ustring) = IK_TRUE_OBJECT;
      *bucket_list_pointer = IK_CDR(s_bucket_list);
      return IK_TRUE_OBJECT;
    } else {
      bucket_list_pointer = (ikptr *)(s_bucket_list + off_cdr);
      s_bucket_list       = *bucket_list_pointer;
    }
  }
#endif
  return IK_FALSE_OBJECT;
}


ikptr
iku_string_to_symbol (ikpcb * pcb, ikptr str)
{
  ikptr s_symbol_table = pcb->symbol_table;
  if (IK_FALSE_OBJECT == s_symbol_table)
    ik_abort("attempt to access dead symbol table");
  if (0 == s_symbol_table) {
    pcb->symbol_table = s_symbol_table = make_symbol_table(pcb);
  }
  return intern_string(str, s_symbol_table, pcb);
}
ikptr
iku_symbol_from_string (ikpcb* pcb, ikptr s_str)
{
  return iku_string_to_symbol(pcb, s_str);
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_string_to_symbol (ikptr str, ikpcb* pcb)
{
  return iku_string_to_symbol(pcb, str);
}
ikptr
ikrt_strings_to_gensym (ikptr s_pretty_string, ikptr s_unique_string, ikpcb* pcb)
{
  ikptr s_gensym_table = pcb->gensym_table;
  if (0 == s_gensym_table) {
    pcb->gensym_table = s_gensym_table = make_symbol_table(pcb);
  }
  return intern_unique_string(s_pretty_string, s_unique_string, s_gensym_table, pcb);
}

/* end of file */

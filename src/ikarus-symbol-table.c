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


static ikptr_t
make_symbol_table (ikpcb_t* pcb)
/* Build and return a new hash table to be used as symbol table for both
   common  symbols and  gensyms.   "Symbol table"  here  means a  Scheme
   vector of  buckets, in which  the value  in each bucket  references a
   proper list  of symbols;  empty bucket slots  are initialised  to the
   fixnum zero.

   The vector is allocated outside  of the memory scanned by the garbage
   collector.  Later  some pages in the  vector may be  registered to be
   scanned. */
{
  ikuword_t	mem_size = IK_ALIGN_TO_NEXT_PAGE(disp_vector_data + NUM_OF_BUCKETS * wordsize);
  ikptr_t	s_symtab = ik_mmap_ptr(mem_size, 0, pcb) | vector_tag;
  /* Here we clear the whole allocated  memory block, which is *not* the
     data area of the vector object. */
  memset((char*)(s_symtab+off_vector_length), '\0', mem_size);
  IK_VECTOR_LENGTH_FX(s_symtab) = IK_FIX(NUM_OF_BUCKETS);
  return s_symtab;
}
ikptr_t
ikrt_get_symbol_table (ikpcb_t* pcb)
/* The symbol  table is created by  C language code,  but, after loading
   the  boot  image,  it is  retrieved  by  Scheme  code to  be  handled
   there. */
{
  ikptr_t	s_symtab = pcb->symbol_table;
  pcb->symbol_table = IK_FALSE_OBJECT;
  if (IK_FALSE == s_symtab)
    ik_abort("attempt to access dead symbol table");
  return s_symtab;
}


static int
strings_eqp (ikptr_t s_str1, ikptr_t s_str2)
{
  ikptr_t	s_len1 = IK_REF(s_str1, off_string_length);
  if (s_len1 == IK_STRING_LENGTH_FX(s_str2)) {
    return (0 == memcmp(IK_STRING_DATA_VOIDP(s_str1),
			IK_STRING_DATA_VOIDP(s_str2),
			IK_UNFIX(s_len1) * IK_STRING_CHAR_SIZE));
  } else
    return 0;
}


/* To compute the  hash value of strings and bytevectors  we use at most
   these number of Scheme characters and bytes from the date area. */
#undef  HASH_GENERATION_CHARS_LIMIT
#define HASH_GENERATION_CHARS_LIMIT	64
#undef  HASH_GENERATION_BYTES_LIMIT
#define HASH_GENERATION_BYTES_LIMIT	256

static ikptr_t
compute_string_hash (ikptr_t str, ikptr_t s_max_len)
/* one-at-a-time from http://burtleburtle.net/bob/hash/doobs.html */
{
  ikptr_t	len  = IK_UNFIX(IK_REF(str, off_string_length));
  ikchar_t *	data = IK_STRING_DATA_IKCHARP(str);
  ikchar_t *	last;
  /* With this initialisation: two strings of different length will have
     different  hash value  even  when  they have  equal  chars used  to
     compute the hash value. */
  ikptr_t	H    = len;
  /* We  expect  S_MAX_LEN to  be:  false,  true, an  already  validated
     non-negative fixnum. */
  {
    ikptr_t	limit;
    if (IK_FALSE == s_max_len) {
      limit = HASH_GENERATION_CHARS_LIMIT;
    } else if (IK_TRUE == s_max_len) {
      limit = len;
    } else {
      limit = IK_UNFIX(s_max_len);
    }
    last  = data + ((len < limit)? len : limit);
  }
  /* one-at-a-time */
  for (; data < last; ++data) {
    ikchar_t	c = IK_CHAR32_TO_INTEGER(*data);
    H = H + c;
    H = H + (H << 10);
    H = H ^ (H >> 6);
  }
  H = H + (H << 3);
  H = H ^ (H >> 11);
  H = H + (H << 15);
  /* Make it positive. */
  return ((H << 4) >> 4);
}
ikptr_t
ikrt_string_hash (ikptr_t str, ikptr_t s_max_len, ikpcb_t * pcb)
{
  return IK_FIX(compute_string_hash(str, s_max_len));
}
ikptr_t
ikrt_bytevector_hash (ikptr_t bv, ikptr_t s_max_len, ikpcb_t * pcb)
{
  ikptr_t	len  = IK_BYTEVECTOR_LENGTH(bv);
  uint8_t *	data = IK_BYTEVECTOR_DATA_UINT8P(bv);
  uint8_t *	last;
  /* With this initialisation: two  bytevectors of different length will
     have different hash  value even when they have equal  bytes used to
     compute the hash value. */
  ikptr_t	H    = len;
  /* We  expect  S_MAX_LEN to  be:  false,  true, an  already  validated
     non-negative fixnum. */
  {
    ikptr_t	limit;
    if (IK_FALSE == s_max_len) {
      limit = HASH_GENERATION_BYTES_LIMIT;
    } else if (IK_TRUE == s_max_len) {
      limit = len;
    } else {
      limit = IK_UNFIX(s_max_len);
    }
    last  = data + ((len < limit)? len : limit);
  }
  /* one-at-a-time */
  for (; data < last; ++data) {
    uint8_t	c = *data;
    H = H + c;
    H = H + (H << 10);
    H = H ^ (H >> 6);
  }
  H = H + (H << 3);
  H = H ^ (H >> 11);
  H = H + (H << 15);
  /* Make it positive. */
  H = ((H << 4) >> 4);
  return IK_FIX(H);
}


static ikptr_t
iku_make_symbol (ikptr_t s_pretty_string, ikptr_t s_unique_string, ikpcb_t* pcb)
{
  ikptr_t s_sym = ik_unsafe_alloc(pcb, symbol_record_size) | record_tag;
  /* There is no need to update the dirty vector about mutating "s_sym":
     "s_sym" has  just been created,  so all the  values that go  in its
     slots are older. */
  IK_REF(s_sym, off_symbol_record_tag)		= symbol_tag;
  IK_REF(s_sym, off_symbol_record_string)	= s_pretty_string;
  IK_REF(s_sym, off_symbol_record_ustring)	= s_unique_string;
  IK_REF(s_sym, off_symbol_record_value)	= IK_UNBOUND_OBJECT;
  IK_REF(s_sym, off_symbol_record_proc)		= IK_UNBOUND_OBJECT;
  IK_REF(s_sym, off_symbol_record_plist)	= IK_NULL_OBJECT;
  return s_sym;
}
static ikptr_t
intern_string (ikptr_t s_unique_string, ikptr_t s_symbol_table, ikpcb_t* pcb)
/* Notice that all the memory allocations here are UNsafe. */
{
  int		hash_value    = compute_string_hash(s_unique_string, IK_TRUE);
  int		bucket_index  = hash_value & (IK_VECTOR_LENGTH(s_symbol_table) - 1);
  ikptr_t	s_bucket_list = IK_ITEM(s_symbol_table, bucket_index);
  { /* If a symbol having S_UNIQUE_STRING is already interned: return it
       to the caller. */
    ikptr_t	s_list_iterator = s_bucket_list;
    while (s_list_iterator && IK_NULL_OBJECT != s_list_iterator) {
      ikptr_t	s_sym     = IK_CAR(s_list_iterator);
      ikptr_t	s_sym_str = IK_REF(s_sym, off_symbol_record_string);
      if (strings_eqp(s_sym_str, s_unique_string)) {
	return s_sym;
      }
      s_list_iterator = IK_CDR(s_list_iterator);
    }
  }
  /* Allocate a new  pointer object and register it  in the symbol table
     by prepending it to the bucket list. */
  ikptr_t s_sym  = iku_make_symbol(s_unique_string, IK_FALSE_OBJECT, pcb);
  ikptr_t s_pair = IKU_PAIR_ALLOC(pcb);
  IK_CAR(s_pair) = s_sym;
  IK_CDR(s_pair) = s_bucket_list;
  IK_ITEM(s_symbol_table, bucket_index) = s_pair;
  { /* Mark the  page containing  the bucket slot  to be scanned  by the
       garbage collector. */
    ikuword_t bucket_slot_pointer = s_symbol_table + off_vector_data + bucket_index * wordsize;
    IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, bucket_slot_pointer);
  }
  return s_sym;
}
static ikptr_t
intern_unique_string (ikptr_t s_pretty_string, ikptr_t s_unique_string, ikptr_t s_symbol_table, ikpcb_t* pcb)
/* Intern a  symbol object, having  S_PRETTY_STRING and S_UNIQUE_STRING,
   in S_SYMBOL_TABLE which can be  either the common symbol table or the
   gensyms table.

   If a symbol object having S_UNIQUE_STRING is already interned: return
   it.  Else: allocate a new symbol object, intern it, return it.

   Notice that all the memory allocations here are UNsafe. */
{
  int   hash_value    = compute_string_hash(s_unique_string, IK_TRUE);
  int   bucket_index  = hash_value & (IK_VECTOR_LENGTH(s_symbol_table) - 1);
  ikptr_t s_bucket_list = IK_ITEM(s_symbol_table, bucket_index);
  { /* If a  symbol having  S_UNIQUE_STRING is already  interned, return
       it. */
    ikptr_t s_list_iterator = s_bucket_list;
    while (s_list_iterator && IK_NULL_OBJECT != s_list_iterator) {
      ikptr_t s_sym      = IK_CAR(s_list_iterator);
      ikptr_t s_sym_ustr = IK_REF(s_sym, off_symbol_record_ustring);
      if (strings_eqp(s_sym_ustr, s_unique_string)) {
	return s_sym;
      }
      s_list_iterator = IK_CDR(s_list_iterator);
    }
  }
  /* Allocate a  new symbol  object and  add it to  the symbol  table by
     prepending it to the bucket list. */
  ikptr_t s_sym  = iku_make_symbol(s_pretty_string, s_unique_string, pcb);
  ikptr_t s_pair = IKU_PAIR_ALLOC(pcb);
  IK_CAR(s_pair) = s_sym;
  IK_CDR(s_pair) = s_bucket_list;
  IK_ITEM(s_symbol_table, bucket_index) = s_pair;
  { /* Mark the  page containing  the bucket slot  to be scanned  by the
       garbage collector. */
    ikuword_t bucket_slot_pointer = s_symbol_table + off_vector_data + bucket_index * wordsize;
    IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb,bucket_slot_pointer);
  }
  return s_sym;
}
ikptr_t
ikrt_intern_gensym (ikptr_t s_sym, ikpcb_t* pcb)
/* Given a  symbol object try to intern  it in the table  of gensyms; if
   successful  return the  true object,  else return  the  false object.
   Successful interning  means that  the unique string  in S_SYM  is not
   already in the table.

   Interning a gensym  cannot fail; this function is  meant to be called
   in a  loop with a  newly generated unique  string stored in  S_SYM at
   each iteration until the interning succeeds. */
{
  ikptr_t s_gensym_table = pcb->gensym_table;
  if (0 == s_gensym_table) {
    pcb->gensym_table = s_gensym_table = make_symbol_table(pcb);
  }
  ikptr_t s_unique_string = IK_REF(s_sym, off_symbol_record_ustring);
  int   hash_value      = compute_string_hash(s_unique_string, IK_TRUE);
  int   bucket_index    = hash_value & (IK_VECTOR_LENGTH(s_gensym_table) - 1);
  ikptr_t s_bucket_list   = IK_ITEM(s_gensym_table, bucket_index);
  { /* If a  symbol having  S_UNIQUE_STRING is already  interned, return
       false. */
    ikptr_t s_list_iterator = s_bucket_list;
    while (s_list_iterator && IK_NULL_OBJECT != s_list_iterator) {
      ikptr_t s_sym      = IK_CAR(s_list_iterator);
      ikptr_t s_sym_ustr = IK_REF(s_sym, off_symbol_record_ustring);
      if (strings_eqp(s_sym_ustr, s_unique_string)) {
	return IK_FALSE_OBJECT;
      }
      s_list_iterator = IK_CDR(s_list_iterator);
    }
  }
  /* Allocate a  new symbol  object and  add it to  the symbol  table by
     prepending it to the bucket list. */
  ikptr_t s_pair = IKU_PAIR_ALLOC(pcb);
  IK_CAR(s_pair) = s_sym;
  IK_CDR(s_pair) = s_bucket_list;
  IK_ITEM(s_gensym_table, bucket_index) = s_pair;
  { /* Mark the  page containing  the bucket slot  to be scanned  by the
       garbage collector. */
    ikuword_t bucket_slot_pointer = s_gensym_table + off_vector_data + bucket_index * wordsize;
    IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb,bucket_slot_pointer);
  }
  return IK_TRUE_OBJECT;
}
ikptr_t
ikrt_unintern_gensym (ikptr_t s_sym, ikpcb_t* pcb)
/* Remove S_SYM  from the  hash table of  interned gensyms;  return true
   object if  the table exists and  S_SYM is present,  else return false
   object. */
{
  /* fprintf(stderr, "removing gensym\n"); */
  ikptr_t gensym_table = pcb->gensym_table;
  if (0 == gensym_table) {
    /* no symbol table */
    return IK_FALSE_OBJECT;
  }
  ikptr_t s_unique_string = IK_REF(s_sym, off_symbol_record_ustring);
  if (IK_TAGOF(s_unique_string) != string_tag) {
    return IK_FALSE_OBJECT;
  }
  int   hash_value    = compute_string_hash(s_unique_string, IK_TRUE);
  int   bucket_index  = hash_value & (IK_VECTOR_LENGTH(gensym_table) - 1);
#if 0
  /* This  is the  original  Ikarus  code.  (Marco  Maggi;  Sun Mar  11,
     2012) */
  ikptr_t bucket_list_pointer = (ikptr_t)(gensym_table + off_vector_data + bucket_index * wordsize);
  ikptr_t s_bucket_list       = IK_REF(bucket_list_pointer, 0);
  while (s_bucket_list) {
    if (IK_CAR(s_bucket_list) == s_sym) {
      /* Found it.   Remove the unique  string from the  gensym.  Remove
	 the containing pair from the bucket list. */
      IK_REF(s_sym, off_symbol_record_ustring) = IK_TRUE_OBJECT;
      IK_REF(bucket_list_pointer, 0) = IK_CDR(s_bucket_list);
      return IK_TRUE_OBJECT;
    } else {
      bucket_list_pointer = (ikptr_t)(s_bucket_list + off_cdr);
      s_bucket_list       = IK_REF(bucket_list_pointer, 0);
    }
  }
#else
  /* This is  the modified  code to make  things more  readable.  (Marco
     Maggi; Sun Mar 11, 2012) */
  ikptr_t * bucket_list_pointer = (ikptr_t *)(gensym_table + off_vector_data + bucket_index * wordsize);
  ikptr_t   s_bucket_list       = *bucket_list_pointer;
  while (s_bucket_list) {
    if (IK_CAR(s_bucket_list) == s_sym) {
      /* Found it.   Remove the unique  string from the  gensym.  Remove
	 the containing pair from the bucket list. */
      IK_REF(s_sym, off_symbol_record_ustring) = IK_TRUE_OBJECT;
      *bucket_list_pointer = IK_CDR(s_bucket_list);
      return IK_TRUE_OBJECT;
    } else {
      bucket_list_pointer = (ikptr_t *)(s_bucket_list + off_cdr);
      s_bucket_list       = *bucket_list_pointer;
    }
  }
#endif
  return IK_FALSE_OBJECT;
}


ikptr_t
iku_string_to_symbol (ikpcb_t * pcb, ikptr_t str)
{
  ikptr_t s_symbol_table = pcb->symbol_table;
  if (IK_FALSE_OBJECT == s_symbol_table)
    ik_abort("attempt to access dead symbol table");
  if (0 == s_symbol_table) {
    pcb->symbol_table = s_symbol_table = make_symbol_table(pcb);
  }
  return intern_string(str, s_symbol_table, pcb);
}
ikptr_t
iku_symbol_from_string (ikpcb_t* pcb, ikptr_t s_str)
{
  return iku_string_to_symbol(pcb, s_str);
}

/* ------------------------------------------------------------------ */

ikptr_t
ikrt_string_to_symbol (ikptr_t str, ikpcb_t* pcb)
{
  return iku_string_to_symbol(pcb, str);
}
ikptr_t
ikrt_strings_to_gensym (ikptr_t s_pretty_string, ikptr_t s_unique_string, ikpcb_t* pcb)
{
  ikptr_t s_gensym_table = pcb->gensym_table;
  if (0 == s_gensym_table) {
    pcb->gensym_table = s_gensym_table = make_symbol_table(pcb);
  }
  return intern_unique_string(s_pretty_string, s_unique_string, s_gensym_table, pcb);
}

/* end of file */

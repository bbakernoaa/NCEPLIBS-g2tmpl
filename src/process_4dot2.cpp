/** @file
 * @brief 4dot2
 * @author Jeff Ator @date Feb 2010
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <fstream>
#include "fkyaml.hpp"

/** Define the maximum length of a mnemonic in GRIB2 Code Table
 * 4.2. */
#define MXG2MNEM 16

/** Define the maximum length of a mnemonic in GRIB2 Code Table
 * 4.2. */
#define MXG2MNEMP4 ( MXG2MNEM + 4)

/** Allocate internal memory for table entries in chunks of 25000. */
#define NUMALLOC 2500

/** Max fn length. */
#define MXFNLEN 120

/** On certain operating systems, the FORTRAN compiler appends an
 *  underscore to subprogram names in its object namespace.
 *  Therefore, on such systems, a matching underscore must be appended
 *  to any C language references to the same subprogram names so that
 *  the linker can correctly resolve such references across the C <->
 *  FORTRAN interface at link time. */
#ifdef UNDERSCORE
#define open_and_read_4dot2 open_and_read_4dot2_
#define sort_and_write_4dot2 sort_and_write_4dot2_
#define search_for_4dot2_entry search_for_4dot2_entry_
#define close_4dot2 close_4dot2_
#endif

/** In order to ensure that the C <-> FORTRAN interface works properly
 *  (and portably!), the default size of an "INTEGER" declared in
 *  FORTRAN must be identical to that of an "int" declared in C. If
 *  this is not the case (e.g.  some FORTRAN compilers, most notably
 *  AIX via the -qintsize= option, allow the sizes of INTEGERs to be
 *  definitively prescribed outside of the source code itself!), then
 *  the following conditional directive (or a variant of it) can be
 *  used to ensure that the size of an "int" in C remains identical to
 *  that of an "INTEGER" in FORTRAN. */
#ifdef F77_INTSIZE_8
typedef long f77int;
#else
typedef int f77int;
#endif

/** Define global variables. */
struct TableEntry {
    char mnemonic[MXG2MNEMP4];
    int discipline;
    int category;
    int parameter;
};

/** Table entry. */
struct TableEntry *pe0 = NULL;

/** Number of entries. */
size_t nentry = 0;

/* Prototypes. */
extern "C" {
void open_and_read_4dot2(char *, f77int *);
void sort_and_write_4dot2(char *, f77int *);
void search_for_4dot2_entry(char nemo[MXG2MNEM], f77int *locflg,
                       f77int *disc, f77int *catg, f77int *parm,
                       f77int *iret);
void close_4dot2(f77int *);
}
int compar(const void *, const void *);

/**
 * Define the internal comparison function for use with qsort and
 * bsearch.
 *
 * @param p1 TableEntry1
 * @param p2 TableEntry2
 *
 * @return 0 if they are the same.
 */
int
compar(const void *p1, const void *p2)
{
    const struct TableEntry *pte1 = (const struct TableEntry *) p1;
    const struct TableEntry *pte2 = (const struct TableEntry *) p2;
    return strcmp(pte1->mnemonic, pte2->mnemonic);
}

/*
**  Define the remaining subprograms which will be called by user applications.
*/

/**
 * Opens and reads the GRIB2 Code Table 4.2 into an internal memory
 * structure.
 *
 * In order for the search_for_4dot2_entry() function to be used, the table
 * must be in sorted order with respect to the mnemonic and according to the
 * criteria of the compar() function. If this has not already been done, it
 * can be done by calling the sort_and_write_4dot2() function prior to calling
 * the search_for_4dot2_entry() function.
 *
 * @param filename Location of table file on filesystem; directory
 * prefixes or other local filesystem notation is allowed up to 120
 * total characters.
 * @param iret Return code:
 * - 0 normal return
 * - -1 input filename was more than 120 characters
 * - -2 table file could not be opened
 * - -3 memory allocation error
 */
extern "C" void
open_and_read_4dot2(char *filename, f77int *iret)
{

    struct TableEntry *pra;

    char lfn[MXFNLEN+1], str[81], cflag, cub = '_';

    size_t i;

    /* Reset global variables in case of multiple calls */
    if (pe0) free(pe0);
    pe0 = NULL;
    nentry = 0;

/*
**  Copy the input filename into a local variable and check it for validity.
**  This is especially important in case the filename was passed in as a
**  string literal by the calling program or else doesn't have a trailing
**  NULL character.
*/
    for (i = 0; (! isspace(filename[i]) && ! iscntrl(filename[i])); i++) {
        if (i == MXFNLEN) {
            *iret = (f77int) -1;
            return;
        }
        lfn[i] = filename[i];
    }
    lfn[i] = '\0';

/*
**  Check if the file is a YAML file.
*/
    if ((strstr(lfn, ".yaml") != NULL) || (strstr(lfn, ".yml") != NULL)) {
        try {
            std::ifstream ifs(lfn);
            if (!ifs.is_open()) {
                *iret = (f77int) -2;
                printf("Can't open input file %s\n", lfn);
                return;
            }
            fkyaml::node root = fkyaml::node::deserialize(ifs);

            if (root.is_sequence()) {
                for (auto& item : root.get_value_ref<fkyaml::node::sequence_type&>()) {
                    if ((nentry % NUMALLOC) == 0) {
                        pra = (struct TableEntry *) realloc(pe0, ((nentry + NUMALLOC) * sizeof(struct TableEntry)));
                        if (pra == NULL) {
                            *iret = (f77int) -3;
                            return;
                        }
                        pe0 = pra;
                        memset(&pe0[nentry], 0, NUMALLOC * sizeof(struct TableEntry));
                    }
                    pe0[nentry].discipline = item["discipline"].template get_value<int>();
                    pe0[nentry].category = item["category"].template get_value<int>();
                    pe0[nentry].parameter = item["parameter"].template get_value<int>();
                    std::string mnem = item["mnemonic"].template get_value<std::string>();
                    int loc = item["local"].template get_value<int>();
                    strncpy(pe0[nentry].mnemonic, mnem.c_str(), MXG2MNEM);
                    pe0[nentry].mnemonic[MXG2MNEM] = '\0';
                    strncat(pe0[nentry].mnemonic, &cub, 1);
                    cflag = (loc == 1) ? '1' : '0';
                    strncat(pe0[nentry].mnemonic, &cflag, 1);
                    nentry++;
                }
            }
            /*
            **  Sort the entries within the internal memory structure to ensure
            **  search_for_4dot2_entry() works correctly even if the file was unsorted.
            */
            if (nentry > 0) {
                qsort(pe0, nentry, sizeof(struct TableEntry), compar);
            }
            *iret = (f77int) 0;
            return;
        } catch (const std::exception& e) {
            printf("Error parsing YAML file %s: %s\n", lfn, e.what());
            *iret = (f77int) -2;
            return;
        }
    }

/*
**  Open the file.
*/
    FILE *pfn;
    if ((pfn = fopen(lfn, "r")) == NULL) {
        *iret = (f77int) -2;
        printf("Can't open input file %s\n",lfn);
        return;
    }

/*
**  Read the file contents into an internal memory structure.  Memory will be
**  allocated as needed for NUMALLOC entries at a time.
*/
    while (fgets(str, 80, pfn) != NULL) {
        if (str[0] != '!') {    /* ignore comment lines */
            if ((nentry % NUMALLOC) == 0) {
/*
**              Allocate additional memory.
*/
                pra = (struct TableEntry *) realloc(pe0, ((nentry + NUMALLOC) * sizeof(struct TableEntry)));
                if (pra == NULL) {
                    *iret = (f77int) -3;
                    fclose(pfn);
                    return;
                }
                pe0 = pra;
                memset(&pe0[nentry], 0, NUMALLOC * sizeof(struct TableEntry));
            }
            sscanf(str, "%d%d%d%*3c%c%*c%s",
                   &pe0[nentry].discipline, &pe0[nentry].category,
                   &pe0[nentry].parameter, &cflag,
                   pe0[nentry].mnemonic);
            strncat(pe0[nentry].mnemonic, &cub, 1);
            strncat(pe0[nentry].mnemonic, &cflag, 1);
            nentry++;
        }
    }

/*
**  Close the file.
*/
    fclose (pfn);

    /*
    **  Sort the entries within the internal memory structure to ensure
    **  search_for_4dot2_entry() works correctly even if the file was unsorted.
    */
    if (nentry > 0) {
        qsort(pe0, nentry, sizeof(struct TableEntry), compar);
    }

    *iret = (f77int) 0;
}

/**
 * Sorts the contents of GRIB2 Code Table 4.2 within the internal memory
 * structure and writes the output to the file specified by filename. The
 * output file will be in a format suitable for subsequent reading via the
 * open_and_read_4dot2() function, so that this function does not need to be
 * called again prior to calling search_for_4dot2_entry().
 *
 * @param filename Filename to which to write the sorted
 * output. Directory prefixes or other local filesystem notation is
 * allowed up to 120 total characters.
 * @param iret Return code:
 * - 0 normal return
 * - -1 filename was more than 120 characters
 * - -2 filename could not be opened
 */
extern "C" void
sort_and_write_4dot2(char *filename, f77int *iret)
{
#define MXFNLEN 120

    char lfn[MXFNLEN+1], str[MXG2MNEMP4], cflag;

    size_t i;

    FILE *pfn;

/*
**  Copy the filename into a local variable and check it for validity.
**  This is especially important in case the filename was passed in as a
**  string literal by the calling program or else doesn't have a trailing
**  NULL character.
*/
    for (i = 0; (! isspace(filename[i]) && ! iscntrl(filename[i])); i++) {
        if (i == MXFNLEN) {
            *iret = (f77int) -1;
            return;
        }
        lfn[i] = filename[i];
    }
    lfn[i] = '\0';

/*
**  Open the output file.
*/
    if ((pfn = fopen(lfn, "w")) == NULL) {
        *iret = (f77int) -2;
        printf("Can't open output file %s\n",lfn);
        return;
    }

/*
**  Sort the entries within the internal memory structure.
*/
    qsort(pe0, nentry, sizeof(struct TableEntry), compar);

/*
**  Write the sorted entries to the output file.
*/
    for (i = 0; i < nentry; i++) {
        strcpy(str, pe0[i].mnemonic);
        cflag = str[strlen(str)-1];
        str[strlen(str)-2] = '\0';
        fprintf(pfn, "%4d%4d%6d   %c %s\n",
                pe0[i].discipline, pe0[i].category,
                pe0[i].parameter, cflag, str);
    }
/*
**  Close the file.
*/
    fclose (pfn);

    *iret = (f77int) 0;
}

/**
 * Searches for a specified mnemonic within the previously-opened
 * GRIB2 Code Table 4.2 and returns the corresponding product
 * discipline, parameter category and parameter number. A binary
 * search algorithm is used.
 *
 * In order for this function to be used, the table must already be in
 * sorted order with respect to the mnemonic and according to the
 * criteria of the compar() function. If this has not already been
 * done, it can be done by calling the sort_and_write_4dot2() function
 * prior to calling this function.
 *
 * @param nemo Mnemonic (of up to 16 characters in length) to search
 * for within table.
 * @param locflg Version of mnemonic to be returned, in case of
 * duplication within table:
 * - 0 international version (default)
 * - 1 local version
 * @param disc Product discipline
 * @param catg Parameter category
 * @param parm Parameter number
 * @param iret Return code:
 * - 0 normal return
 * - -1 nemo not found within table for specified locflg version
 */
extern "C" void
search_for_4dot2_entry(char nemo[MXG2MNEM], f77int *locflg,
                       f77int *disc, f77int *catg, f77int *parm,
                       f77int *iret)
{
    unsigned short n = 0, n2 = 0;

    long llf;

    struct TableEntry key, *pbs;

    size_t ipt;

/*
**  Make a local copy of nemo.  Mnemonics may consist of any combination of
**  alphanumeric, underscore and dash characters.
*/
    while ( (n < MXG2MNEM) &&
            ((isalnum((int) nemo[n]) ||
              (nemo[n] == '_') || (nemo[n] == '-'))) ) {
        key.mnemonic[n2++] = nemo[n++];
    }

/*
**  Append an underscore followed by the locflg in order to generate the
**  mnemonic to search for.
*/
    key.mnemonic[n2++] = '_';
    llf = (long) *locflg;
    if (llf != 1) llf = 0;   /* default to using international entry unless
                                local is specified */
    sprintf(&key.mnemonic[n2], "%ld", llf);  /* trailing null will be automatically
                                                appended by sprintf */

/*
**  Search for the mnemonic in the Code Table and return appropriate output values.
*/
    pbs = (struct TableEntry *) bsearch(&key, pe0, nentry, sizeof(struct TableEntry), compar);
    if (pbs == NULL) {
        *iret = (f77int) -1;
    }
    else {
        *iret = (f77int) 0;
        ipt = pbs - pe0;
        *disc = (f77int) pe0[ipt].discipline;
        *catg = (f77int) pe0[ipt].category;
        *parm = (f77int) pe0[ipt].parameter;
    }

}

/**
 * This subroutine should be called one time at the end of the application
 * program in order to free all allocated memory.
 *
 * @param iret Return code: 0 = normal return.
 */
extern "C" void
close_4dot2(f77int *iret)
{
    if (pe0) free (pe0);
    pe0 = NULL;
    nentry = 0;
    *iret = (f77int) 0;
}

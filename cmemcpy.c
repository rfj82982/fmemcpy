/** cmemcpy.c
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <string.h>

void cmemcpy(void *dst, void *src, size_t n)
{
  memcpy(dst, src, n);
}

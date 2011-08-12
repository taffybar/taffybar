////////////////////////////////////////////////////////////////////////////
// Copyright   :  (c) Jan Vornberger 2009
// License     :  BSD3-style (see LICENSE)
//
// Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
////////////////////////////////////////////////////////////////////////////-

#include <gtk/gtk.h>
#include <gdk/gdk.h>

void set_strut_properties(GtkWindow *window,
				long left, long right, long top, long bottom,
 				long left_start_y, long left_end_y,
 				long right_start_y, long right_end_y,
 				long top_start_x, long top_end_x,
 				long bottom_start_x, long bottom_end_x) {
	gulong data[12] = {0};
	data[0] = left; data[1] = right; data[2] = top; data[3] = bottom;
	data[4] = left_start_y; data[5] = left_end_y;
	data[6] = right_start_y; data[7] = right_end_y;
	data[8] = top_start_x; data[9] = top_end_x;
	data[10] = bottom_start_x; data[11] = bottom_end_x;

	gdk_property_change(GTK_WIDGET(window)->window,
				gdk_atom_intern("_NET_WM_STRUT_PARTIAL", FALSE),
				gdk_atom_intern ("CARDINAL", FALSE),
				32, GDK_PROP_MODE_REPLACE, (unsigned char *)data, 12);
}

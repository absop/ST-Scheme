from .pretty.pretty import pretty
import sublime
import sublime_plugin


def get_region_indent_level(view, region):
    start_pt = region.begin()
    row, col = view.rowcol(start_pt)
    return col

class SchemeCodeFormatterCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        try:
            self.format_regions(edit)
            sublime.status_message("formated")
        except Exception as e:
            sublime.status_message(str(e))

    def format_regions(self, edit):
        regions = [s for s in self.view.sel()]
        view_encoding = self.view.encoding()
        if view_encoding == 'Undefined':
            view_encoding = 'utf-8'

        for region in reversed(regions):
            if region.size() < 4: continue

            indent_level = get_region_indent_level(self.view, region)
            original_code = self.view.substr(region)
            prettied_code = pretty(original_code, indent_level)
            if prettied_code != original_code:
                self.view.replace(edit, region, prettied_code)

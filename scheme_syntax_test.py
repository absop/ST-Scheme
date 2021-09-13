import os
import time
import threading
import sublime_api
import sublime
import sublime_plugin


class ProfileSchemeSyntaxCommand(sublime_plugin.WindowCommand):
    def run(self, **kwargs):
        if not hasattr(self, 'output_view'):
            # Try not to call get_output_panel until the regexes are assigned
            self.output_view = self.window.create_output_panel('exec')

        self.output_view.settings().set('line_numbers', False)
        self.output_view.settings().set('gutter', False)
        self.output_view.settings().set('scroll_past_end', False)

        # Call create_output_panel a second time after assigning the above
        # settings, so that it'll be picked up as a result buffer
        self.window.create_output_panel('exec')

        show_panel_on_build(self.window)
        output_profile(self.output_view,
            [ 'Packages/Scheme/syntax/Scheme.sublime-syntax'
            ],
            kwargs.get('times', 1))


def output_profile(output_view, syntaxes, times):
    start_time = time.time()
    profiles = {s: [] for s in syntaxes}
    os.chdir(os.path.join(os.path.dirname(__file__), '.scheme'))
    for file in os.listdir():
        source = open(file, encoding='utf-8').read()
        for syntax in syntaxes:
            total = 0.0
            for _ in range(times):
                total += sublime_api.profile_syntax_definition(source, syntax)
            avg = total / times * 1000
            profiles[syntax].append((file, avg))
    for syntax in profiles:
        append(output_view, f'Syntax: {syntax}\n')
        for file, avg in profiles[syntax]:
            append(output_view, f'{avg:10.2f} ms for {file}\n')
    elapsed = time.time() - start_time

    if elapsed < 1:
        elapsed_str = "%.0fms" % (elapsed * 1000)
    else:
        elapsed_str = "%.1fs" % (elapsed)
    append(output_view, f"[Finished in {elapsed_str}]")


def show_panel_on_build(window):
    if sublime.load_settings('Preferences.sublime-settings').get(
            'show_panel_on_build', True):
        window.run_command('show_panel', {'panel': 'output.exec'})


def append(panel, output):
    panel.run_command('append', {'characters': output})


"""
Syntax: Packages/Scheme/syntax/Scheme.sublime-syntax
    6.24 ms for church-compile.ss
   42.69 ms for cmacros.ss
   66.45 ms for compiler.ss
  361.01 ms for cpnanopass.ss
    8.84 ms for match.ss
   14.17 ms for pretty.ss
 1445.17 ms for prettytest.ss
    6.28 ms for quasiquote.ss
   35.67 ms for read.ss
  194.21 ms for syntax.ss
    5.73 ms for test.ss
    6.47 ms for while-loop.ss
"""

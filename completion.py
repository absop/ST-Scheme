import os
import time
import subprocess
from subprocess import Popen, PIPE

import sublime
import sublime_plugin


os_is_windows = os.name == 'nt'
chez_scheme_proc = None


def plugin_unloaded():
    if scheme_is_in_place():
        kill_process(chez_scheme_proc)

def scheme_is_in_place():
    return chez_scheme_proc and chez_scheme_proc.poll() is None

# 如果你打开任务管理器并选中Sublime，然后关闭 sublime，
# 子进程不会被结束，而是在后台大量虚耗CPU
def kill_process(proc):
    if os_is_windows:
        killer = "TASKKILL /F /PID {pid} /T"
        Popen(killer.format(pid=proc.pid), shell=True)
    else:
        os.killpg(os.getpgid(proc.pid), signal.SIGTERM)
    print("[Scheme.CodeHelper] terminated scheme process")

def check_scheme_process():
    global chez_scheme_proc
    if not scheme_is_in_place():
        startupinfo = None
        if os_is_windows:
            startupinfo = subprocess.STARTUPINFO()
            startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        proc = Popen("scheme", stdin=PIPE, stdout=PIPE, stderr=PIPE,
                     shell=True, startupinfo=startupinfo)
        proc.stdout.read1(256)
        chez_scheme_proc = proc
        print("[Scheme.CodeHelper] created scheme process")

    return chez_scheme_proc


class SchemeCodeHelperListener(sublime_plugin.EventListener):
    view_environments = {}

    def on_query_completions(self, view, prefix, locations):
        """(environment-symbols env-{view_view_id})
        """
        pass

    def on_load(self, view):
        """(define env-{view_view_id} (copy-environment (scheme-environment)))
        """
        pass

    def on_modified(self, view):
        """(eval {expr} env-{view.view_id})"""
        pass

    def on_close(self, view):
        """(set! env-{view_view_id} #f)"""
        pass

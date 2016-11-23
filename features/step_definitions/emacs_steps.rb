require 'aruba/cucumber'

When(/^I invoke (\S+) on (\S+)$/) do | function, feature_file |
    cmd = %<emacs --batch -l ../../feature-mode.el #{feature_file} --eval '(progn (feature-mode) (#{function}) (set-buffer "*compilation*") (princ (buffer-string)))'>
    step %(I run `#{cmd}`)
end

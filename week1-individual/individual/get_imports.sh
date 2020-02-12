perl -0ne 'while (/import\s(((\w*)\.){1,})(\w+)/g) {print "$4\n";}'

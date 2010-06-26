Summary:   TclAdaShell
Name:      tash
Version:   8.6
Release:   0
Group:     Ada bindings
License:   GNAT modified GPL (GMGPL)
URL:       http://sourceforge.net/projects/tcladashell
Prefix:    %{InstallPath}

%define _sourcedir .
%define _specdir .
%define _srcrpmdir .
%define _builddir .
%define _rpmdir .
%define _rpmfilename %{name}-%{version}-%{release}.%{arch}.rpm
%define installdir /%{prefix}

%description
This package provides TASH, the Ada binding to Tcl/Tk.

%prep

%build

%install
# Create directory structure and copy files.
install -d %buildroot%installdir/{include,lib}
install -p -m 644 include/*.ad? %buildroot%installdir/include
install -p -m 644 lib/*.* %buildroot%installdir/lib

%post
echo "export TASH_ROOT=%installdir" > /etc/profile.d/tash.sh
echo "setenv TASH_ROOT %installdir" > /etc/profile.d/tash.csh
chmod a+r /etc/profile.d/tash.sh /etc/profile.d/tash.csh

%clean

%postun

%files
%defattr(-,root,root)
%dir %attr(755,root,root) %installdir/include/*
%dir %attr(755,root,root) %installdir/lib/*

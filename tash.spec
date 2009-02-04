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
mkdir -p %buildroot%installdir/{include,lib}
cp -p include/*.ad? %buildroot%installdir/include
cp -p lib/*.* %buildroot%installdir/lib

%post

%clean

%postun

%files
%defattr(-,root,root)
%dir %attr(755,root,root) %installdir/include/*
%dir %attr(755,root,root) %installdir/lib/*

\name{getVoxelSphere}
\alias{getVoxelSphere}
\title{Get Spherical ROI coordinates surrounding 3D voxel}
\description{Get Spherical ROI coordinates surrounding 3D voxel}
\usage{getVoxelSphere(sphereRadius, voxelSz, center, volumeDims)}
\arguments{
  \item{sphereRadius}{Radius of desired spherical ROI in mm}
  \item{voxelSz}{Size of each voxel in mm}
  \item{center}{X,Y,Z coordinates of ROI center}
  \item{volumeDims}{Dimensions of 3D volume, overal X,Y,Z}
}
\value{Returns an n x 3 matrix with x,y,z (columns) coordinates of all voxels within the specified sphere.  Useful for searchlight analyses.
}
\author{Timothy R. Koscik <timkoscik+rmisc@gmail.com>}
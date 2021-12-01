{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.Types.DevEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DevEndpoint where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.WorkerType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A development endpoint where a developer can remotely debug extract,
-- transform, and load (ETL) scripts.
--
-- /See:/ 'newDevEndpoint' smart constructor.
data DevEndpoint = DevEndpoint'
  { -- | The current status of this @DevEndpoint@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The reason for a current failure in this @DevEndpoint@.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the @DevEndpoint@.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | The number of workers of a defined @workerType@ that are allocated to
    -- the development endpoint.
    --
    -- The maximum number of workers you can define are 299 for @G.1X@, and 149
    -- for @G.2X@.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The paths to one or more Python libraries in an Amazon S3 bucket that
    -- should be loaded in your @DevEndpoint@. Multiple values must be complete
    -- paths separated by a comma.
    --
    -- You can only use pure Python libraries with a @DevEndpoint@. Libraries
    -- that rely on C extensions, such as the
    -- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
    -- currently supported.
    extraPythonLibsS3Path :: Prelude.Maybe Prelude.Text,
    -- | The status of the last update.
    lastUpdateStatus :: Prelude.Maybe Prelude.Text,
    -- | A list of security group identifiers used in this @DevEndpoint@.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The point in time at which this @DevEndpoint@ was last modified.
    lastModifiedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | A list of public keys to be used by the @DevEndpoints@ for
    -- authentication. Using this attribute is preferred over a single public
    -- key because the public keys allow you to have a different private key
    -- per client.
    --
    -- If you previously created an endpoint with a public key, you must remove
    -- that key to be able to set a list of public keys. Call the
    -- @UpdateDevEndpoint@ API operation with the public key content in the
    -- @deletePublicKeys@ attribute, and the list of new keys in the
    -- @addPublicKeys@ attribute.
    publicKeys :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | A map of arguments used to configure the @DevEndpoint@.
    --
    -- Valid arguments are:
    --
    -- -   @\"--enable-glue-datacatalog\": \"\"@
    --
    -- You can specify a version of Python support for development endpoints by
    -- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
    -- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
    -- defaults to Python 2.
    arguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A private IP address to access the @DevEndpoint@ within a VPC if the
    -- @DevEndpoint@ is created within one. The @PrivateAddress@ field is
    -- present only when you create the @DevEndpoint@ within your VPC.
    privateAddress :: Prelude.Maybe Prelude.Text,
    -- | The type of predefined worker that is allocated to the development
    -- endpoint. Accepts a value of Standard, G.1X, or G.2X.
    --
    -- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
    --     of memory and a 50GB disk, and 2 executors per worker.
    --
    -- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
    --     of memory, 64 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for memory-intensive jobs.
    --
    -- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
    --     of memory, 128 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for memory-intensive jobs.
    --
    -- Known issue: when a development endpoint is created with the @G.2X@
    -- @WorkerType@ configuration, the Spark drivers for the development
    -- endpoint will run on 4 vCPU, 16 GB of memory, and a 64 GB disk.
    workerType :: Prelude.Maybe WorkerType,
    -- | The name of the @SecurityConfiguration@ structure to be used with this
    -- @DevEndpoint@.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The public key to be used by this @DevEndpoint@ for authentication. This
    -- attribute is provided for backward compatibility because the recommended
    -- attribute to use is public keys.
    publicKey :: Prelude.Maybe Prelude.Text,
    -- | The subnet ID for this @DevEndpoint@.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | Glue version determines the versions of Apache Spark and Python that
    -- Glue supports. The Python version indicates the version supported for
    -- running your ETL scripts on development endpoints.
    --
    -- For more information about the available Glue versions and corresponding
    -- Spark and Python versions, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
    -- in the developer guide.
    --
    -- Development endpoints that are created without specifying a Glue version
    -- default to Glue 0.9.
    --
    -- You can specify a version of Python support for development endpoints by
    -- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
    -- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
    -- defaults to Python 2.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | The number of Glue Data Processing Units (DPUs) allocated to this
    -- @DevEndpoint@.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The public IP address used by this @DevEndpoint@. The @PublicAddress@
    -- field is present only when you create a non-virtual private cloud (VPC)
    -- @DevEndpoint@.
    publicAddress :: Prelude.Maybe Prelude.Text,
    -- | The AWS Availability Zone where this @DevEndpoint@ is located.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The Apache Zeppelin port for the remote Apache Spark interpreter.
    zeppelinRemoteSparkInterpreterPort :: Prelude.Maybe Prelude.Int,
    -- | The path to one or more Java @.jar@ files in an S3 bucket that should be
    -- loaded in your @DevEndpoint@.
    --
    -- You can only use pure Java\/Scala libraries with a @DevEndpoint@.
    extraJarsS3Path :: Prelude.Maybe Prelude.Text,
    -- | The point in time at which this DevEndpoint was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The YARN endpoint address used by this @DevEndpoint@.
    yarnEndpointAddress :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role used in this
    -- @DevEndpoint@.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DevEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'devEndpoint_status' - The current status of this @DevEndpoint@.
--
-- 'failureReason', 'devEndpoint_failureReason' - The reason for a current failure in this @DevEndpoint@.
--
-- 'endpointName', 'devEndpoint_endpointName' - The name of the @DevEndpoint@.
--
-- 'numberOfWorkers', 'devEndpoint_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated to
-- the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
--
-- 'extraPythonLibsS3Path', 'devEndpoint_extraPythonLibsS3Path' - The paths to one or more Python libraries in an Amazon S3 bucket that
-- should be loaded in your @DevEndpoint@. Multiple values must be complete
-- paths separated by a comma.
--
-- You can only use pure Python libraries with a @DevEndpoint@. Libraries
-- that rely on C extensions, such as the
-- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
-- currently supported.
--
-- 'lastUpdateStatus', 'devEndpoint_lastUpdateStatus' - The status of the last update.
--
-- 'securityGroupIds', 'devEndpoint_securityGroupIds' - A list of security group identifiers used in this @DevEndpoint@.
--
-- 'lastModifiedTimestamp', 'devEndpoint_lastModifiedTimestamp' - The point in time at which this @DevEndpoint@ was last modified.
--
-- 'publicKeys', 'devEndpoint_publicKeys' - A list of public keys to be used by the @DevEndpoints@ for
-- authentication. Using this attribute is preferred over a single public
-- key because the public keys allow you to have a different private key
-- per client.
--
-- If you previously created an endpoint with a public key, you must remove
-- that key to be able to set a list of public keys. Call the
-- @UpdateDevEndpoint@ API operation with the public key content in the
-- @deletePublicKeys@ attribute, and the list of new keys in the
-- @addPublicKeys@ attribute.
--
-- 'vpcId', 'devEndpoint_vpcId' - The ID of the virtual private cloud (VPC) used by this @DevEndpoint@.
--
-- 'arguments', 'devEndpoint_arguments' - A map of arguments used to configure the @DevEndpoint@.
--
-- Valid arguments are:
--
-- -   @\"--enable-glue-datacatalog\": \"\"@
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
--
-- 'privateAddress', 'devEndpoint_privateAddress' - A private IP address to access the @DevEndpoint@ within a VPC if the
-- @DevEndpoint@ is created within one. The @PrivateAddress@ field is
-- present only when you create the @DevEndpoint@ within your VPC.
--
-- 'workerType', 'devEndpoint_workerType' - The type of predefined worker that is allocated to the development
-- endpoint. Accepts a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
--     of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
--     of memory, 128 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- Known issue: when a development endpoint is created with the @G.2X@
-- @WorkerType@ configuration, the Spark drivers for the development
-- endpoint will run on 4 vCPU, 16 GB of memory, and a 64 GB disk.
--
-- 'securityConfiguration', 'devEndpoint_securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this
-- @DevEndpoint@.
--
-- 'publicKey', 'devEndpoint_publicKey' - The public key to be used by this @DevEndpoint@ for authentication. This
-- attribute is provided for backward compatibility because the recommended
-- attribute to use is public keys.
--
-- 'subnetId', 'devEndpoint_subnetId' - The subnet ID for this @DevEndpoint@.
--
-- 'glueVersion', 'devEndpoint_glueVersion' - Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- running your ETL scripts on development endpoints.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- Development endpoints that are created without specifying a Glue version
-- default to Glue 0.9.
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
--
-- 'numberOfNodes', 'devEndpoint_numberOfNodes' - The number of Glue Data Processing Units (DPUs) allocated to this
-- @DevEndpoint@.
--
-- 'publicAddress', 'devEndpoint_publicAddress' - The public IP address used by this @DevEndpoint@. The @PublicAddress@
-- field is present only when you create a non-virtual private cloud (VPC)
-- @DevEndpoint@.
--
-- 'availabilityZone', 'devEndpoint_availabilityZone' - The AWS Availability Zone where this @DevEndpoint@ is located.
--
-- 'zeppelinRemoteSparkInterpreterPort', 'devEndpoint_zeppelinRemoteSparkInterpreterPort' - The Apache Zeppelin port for the remote Apache Spark interpreter.
--
-- 'extraJarsS3Path', 'devEndpoint_extraJarsS3Path' - The path to one or more Java @.jar@ files in an S3 bucket that should be
-- loaded in your @DevEndpoint@.
--
-- You can only use pure Java\/Scala libraries with a @DevEndpoint@.
--
-- 'createdTimestamp', 'devEndpoint_createdTimestamp' - The point in time at which this DevEndpoint was created.
--
-- 'yarnEndpointAddress', 'devEndpoint_yarnEndpointAddress' - The YARN endpoint address used by this @DevEndpoint@.
--
-- 'roleArn', 'devEndpoint_roleArn' - The Amazon Resource Name (ARN) of the IAM role used in this
-- @DevEndpoint@.
newDevEndpoint ::
  DevEndpoint
newDevEndpoint =
  DevEndpoint'
    { status = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      extraPythonLibsS3Path = Prelude.Nothing,
      lastUpdateStatus = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      publicKeys = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      arguments = Prelude.Nothing,
      privateAddress = Prelude.Nothing,
      workerType = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      publicKey = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      publicAddress = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      zeppelinRemoteSparkInterpreterPort = Prelude.Nothing,
      extraJarsS3Path = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      yarnEndpointAddress = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The current status of this @DevEndpoint@.
devEndpoint_status :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_status = Lens.lens (\DevEndpoint' {status} -> status) (\s@DevEndpoint' {} a -> s {status = a} :: DevEndpoint)

-- | The reason for a current failure in this @DevEndpoint@.
devEndpoint_failureReason :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_failureReason = Lens.lens (\DevEndpoint' {failureReason} -> failureReason) (\s@DevEndpoint' {} a -> s {failureReason = a} :: DevEndpoint)

-- | The name of the @DevEndpoint@.
devEndpoint_endpointName :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_endpointName = Lens.lens (\DevEndpoint' {endpointName} -> endpointName) (\s@DevEndpoint' {} a -> s {endpointName = a} :: DevEndpoint)

-- | The number of workers of a defined @workerType@ that are allocated to
-- the development endpoint.
--
-- The maximum number of workers you can define are 299 for @G.1X@, and 149
-- for @G.2X@.
devEndpoint_numberOfWorkers :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Int)
devEndpoint_numberOfWorkers = Lens.lens (\DevEndpoint' {numberOfWorkers} -> numberOfWorkers) (\s@DevEndpoint' {} a -> s {numberOfWorkers = a} :: DevEndpoint)

-- | The paths to one or more Python libraries in an Amazon S3 bucket that
-- should be loaded in your @DevEndpoint@. Multiple values must be complete
-- paths separated by a comma.
--
-- You can only use pure Python libraries with a @DevEndpoint@. Libraries
-- that rely on C extensions, such as the
-- <http://pandas.pydata.org/ pandas> Python data analysis library, are not
-- currently supported.
devEndpoint_extraPythonLibsS3Path :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_extraPythonLibsS3Path = Lens.lens (\DevEndpoint' {extraPythonLibsS3Path} -> extraPythonLibsS3Path) (\s@DevEndpoint' {} a -> s {extraPythonLibsS3Path = a} :: DevEndpoint)

-- | The status of the last update.
devEndpoint_lastUpdateStatus :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_lastUpdateStatus = Lens.lens (\DevEndpoint' {lastUpdateStatus} -> lastUpdateStatus) (\s@DevEndpoint' {} a -> s {lastUpdateStatus = a} :: DevEndpoint)

-- | A list of security group identifiers used in this @DevEndpoint@.
devEndpoint_securityGroupIds :: Lens.Lens' DevEndpoint (Prelude.Maybe [Prelude.Text])
devEndpoint_securityGroupIds = Lens.lens (\DevEndpoint' {securityGroupIds} -> securityGroupIds) (\s@DevEndpoint' {} a -> s {securityGroupIds = a} :: DevEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The point in time at which this @DevEndpoint@ was last modified.
devEndpoint_lastModifiedTimestamp :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.UTCTime)
devEndpoint_lastModifiedTimestamp = Lens.lens (\DevEndpoint' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@DevEndpoint' {} a -> s {lastModifiedTimestamp = a} :: DevEndpoint) Prelude.. Lens.mapping Core._Time

-- | A list of public keys to be used by the @DevEndpoints@ for
-- authentication. Using this attribute is preferred over a single public
-- key because the public keys allow you to have a different private key
-- per client.
--
-- If you previously created an endpoint with a public key, you must remove
-- that key to be able to set a list of public keys. Call the
-- @UpdateDevEndpoint@ API operation with the public key content in the
-- @deletePublicKeys@ attribute, and the list of new keys in the
-- @addPublicKeys@ attribute.
devEndpoint_publicKeys :: Lens.Lens' DevEndpoint (Prelude.Maybe [Prelude.Text])
devEndpoint_publicKeys = Lens.lens (\DevEndpoint' {publicKeys} -> publicKeys) (\s@DevEndpoint' {} a -> s {publicKeys = a} :: DevEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the virtual private cloud (VPC) used by this @DevEndpoint@.
devEndpoint_vpcId :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_vpcId = Lens.lens (\DevEndpoint' {vpcId} -> vpcId) (\s@DevEndpoint' {} a -> s {vpcId = a} :: DevEndpoint)

-- | A map of arguments used to configure the @DevEndpoint@.
--
-- Valid arguments are:
--
-- -   @\"--enable-glue-datacatalog\": \"\"@
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
devEndpoint_arguments :: Lens.Lens' DevEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
devEndpoint_arguments = Lens.lens (\DevEndpoint' {arguments} -> arguments) (\s@DevEndpoint' {} a -> s {arguments = a} :: DevEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | A private IP address to access the @DevEndpoint@ within a VPC if the
-- @DevEndpoint@ is created within one. The @PrivateAddress@ field is
-- present only when you create the @DevEndpoint@ within your VPC.
devEndpoint_privateAddress :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_privateAddress = Lens.lens (\DevEndpoint' {privateAddress} -> privateAddress) (\s@DevEndpoint' {} a -> s {privateAddress = a} :: DevEndpoint)

-- | The type of predefined worker that is allocated to the development
-- endpoint. Accepts a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
--     of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
--     of memory, 128 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- Known issue: when a development endpoint is created with the @G.2X@
-- @WorkerType@ configuration, the Spark drivers for the development
-- endpoint will run on 4 vCPU, 16 GB of memory, and a 64 GB disk.
devEndpoint_workerType :: Lens.Lens' DevEndpoint (Prelude.Maybe WorkerType)
devEndpoint_workerType = Lens.lens (\DevEndpoint' {workerType} -> workerType) (\s@DevEndpoint' {} a -> s {workerType = a} :: DevEndpoint)

-- | The name of the @SecurityConfiguration@ structure to be used with this
-- @DevEndpoint@.
devEndpoint_securityConfiguration :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_securityConfiguration = Lens.lens (\DevEndpoint' {securityConfiguration} -> securityConfiguration) (\s@DevEndpoint' {} a -> s {securityConfiguration = a} :: DevEndpoint)

-- | The public key to be used by this @DevEndpoint@ for authentication. This
-- attribute is provided for backward compatibility because the recommended
-- attribute to use is public keys.
devEndpoint_publicKey :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_publicKey = Lens.lens (\DevEndpoint' {publicKey} -> publicKey) (\s@DevEndpoint' {} a -> s {publicKey = a} :: DevEndpoint)

-- | The subnet ID for this @DevEndpoint@.
devEndpoint_subnetId :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_subnetId = Lens.lens (\DevEndpoint' {subnetId} -> subnetId) (\s@DevEndpoint' {} a -> s {subnetId = a} :: DevEndpoint)

-- | Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The Python version indicates the version supported for
-- running your ETL scripts on development endpoints.
--
-- For more information about the available Glue versions and corresponding
-- Spark and Python versions, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job.html Glue version>
-- in the developer guide.
--
-- Development endpoints that are created without specifying a Glue version
-- default to Glue 0.9.
--
-- You can specify a version of Python support for development endpoints by
-- using the @Arguments@ parameter in the @CreateDevEndpoint@ or
-- @UpdateDevEndpoint@ APIs. If no arguments are provided, the version
-- defaults to Python 2.
devEndpoint_glueVersion :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_glueVersion = Lens.lens (\DevEndpoint' {glueVersion} -> glueVersion) (\s@DevEndpoint' {} a -> s {glueVersion = a} :: DevEndpoint)

-- | The number of Glue Data Processing Units (DPUs) allocated to this
-- @DevEndpoint@.
devEndpoint_numberOfNodes :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Int)
devEndpoint_numberOfNodes = Lens.lens (\DevEndpoint' {numberOfNodes} -> numberOfNodes) (\s@DevEndpoint' {} a -> s {numberOfNodes = a} :: DevEndpoint)

-- | The public IP address used by this @DevEndpoint@. The @PublicAddress@
-- field is present only when you create a non-virtual private cloud (VPC)
-- @DevEndpoint@.
devEndpoint_publicAddress :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_publicAddress = Lens.lens (\DevEndpoint' {publicAddress} -> publicAddress) (\s@DevEndpoint' {} a -> s {publicAddress = a} :: DevEndpoint)

-- | The AWS Availability Zone where this @DevEndpoint@ is located.
devEndpoint_availabilityZone :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_availabilityZone = Lens.lens (\DevEndpoint' {availabilityZone} -> availabilityZone) (\s@DevEndpoint' {} a -> s {availabilityZone = a} :: DevEndpoint)

-- | The Apache Zeppelin port for the remote Apache Spark interpreter.
devEndpoint_zeppelinRemoteSparkInterpreterPort :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Int)
devEndpoint_zeppelinRemoteSparkInterpreterPort = Lens.lens (\DevEndpoint' {zeppelinRemoteSparkInterpreterPort} -> zeppelinRemoteSparkInterpreterPort) (\s@DevEndpoint' {} a -> s {zeppelinRemoteSparkInterpreterPort = a} :: DevEndpoint)

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be
-- loaded in your @DevEndpoint@.
--
-- You can only use pure Java\/Scala libraries with a @DevEndpoint@.
devEndpoint_extraJarsS3Path :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_extraJarsS3Path = Lens.lens (\DevEndpoint' {extraJarsS3Path} -> extraJarsS3Path) (\s@DevEndpoint' {} a -> s {extraJarsS3Path = a} :: DevEndpoint)

-- | The point in time at which this DevEndpoint was created.
devEndpoint_createdTimestamp :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.UTCTime)
devEndpoint_createdTimestamp = Lens.lens (\DevEndpoint' {createdTimestamp} -> createdTimestamp) (\s@DevEndpoint' {} a -> s {createdTimestamp = a} :: DevEndpoint) Prelude.. Lens.mapping Core._Time

-- | The YARN endpoint address used by this @DevEndpoint@.
devEndpoint_yarnEndpointAddress :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_yarnEndpointAddress = Lens.lens (\DevEndpoint' {yarnEndpointAddress} -> yarnEndpointAddress) (\s@DevEndpoint' {} a -> s {yarnEndpointAddress = a} :: DevEndpoint)

-- | The Amazon Resource Name (ARN) of the IAM role used in this
-- @DevEndpoint@.
devEndpoint_roleArn :: Lens.Lens' DevEndpoint (Prelude.Maybe Prelude.Text)
devEndpoint_roleArn = Lens.lens (\DevEndpoint' {roleArn} -> roleArn) (\s@DevEndpoint' {} a -> s {roleArn = a} :: DevEndpoint)

instance Core.FromJSON DevEndpoint where
  parseJSON =
    Core.withObject
      "DevEndpoint"
      ( \x ->
          DevEndpoint'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "EndpointName")
            Prelude.<*> (x Core..:? "NumberOfWorkers")
            Prelude.<*> (x Core..:? "ExtraPythonLibsS3Path")
            Prelude.<*> (x Core..:? "LastUpdateStatus")
            Prelude.<*> ( x Core..:? "SecurityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LastModifiedTimestamp")
            Prelude.<*> (x Core..:? "PublicKeys" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "Arguments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "PrivateAddress")
            Prelude.<*> (x Core..:? "WorkerType")
            Prelude.<*> (x Core..:? "SecurityConfiguration")
            Prelude.<*> (x Core..:? "PublicKey")
            Prelude.<*> (x Core..:? "SubnetId")
            Prelude.<*> (x Core..:? "GlueVersion")
            Prelude.<*> (x Core..:? "NumberOfNodes")
            Prelude.<*> (x Core..:? "PublicAddress")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "ZeppelinRemoteSparkInterpreterPort")
            Prelude.<*> (x Core..:? "ExtraJarsS3Path")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "YarnEndpointAddress")
            Prelude.<*> (x Core..:? "RoleArn")
      )

instance Prelude.Hashable DevEndpoint where
  hashWithSalt salt' DevEndpoint' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` yarnEndpointAddress
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` extraJarsS3Path
      `Prelude.hashWithSalt` zeppelinRemoteSparkInterpreterPort
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` publicAddress
      `Prelude.hashWithSalt` numberOfNodes
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` publicKey
      `Prelude.hashWithSalt` securityConfiguration
      `Prelude.hashWithSalt` workerType
      `Prelude.hashWithSalt` privateAddress
      `Prelude.hashWithSalt` arguments
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` publicKeys
      `Prelude.hashWithSalt` lastModifiedTimestamp
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` lastUpdateStatus
      `Prelude.hashWithSalt` extraPythonLibsS3Path
      `Prelude.hashWithSalt` numberOfWorkers
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` status

instance Prelude.NFData DevEndpoint where
  rnf DevEndpoint' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf yarnEndpointAddress
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf extraJarsS3Path
      `Prelude.seq` Prelude.rnf zeppelinRemoteSparkInterpreterPort
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf publicAddress
      `Prelude.seq` Prelude.rnf numberOfNodes
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf publicKey
      `Prelude.seq` Prelude.rnf securityConfiguration
      `Prelude.seq` Prelude.rnf workerType
      `Prelude.seq` Prelude.rnf privateAddress
      `Prelude.seq` Prelude.rnf arguments
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf publicKeys
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf lastUpdateStatus
      `Prelude.seq` Prelude.rnf extraPythonLibsS3Path
      `Prelude.seq` Prelude.rnf numberOfWorkers
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf failureReason

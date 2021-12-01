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
-- Module      : Amazonka.Neptune.Types.OrderableDBInstanceOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.OrderableDBInstanceOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Neptune.Types.AvailabilityZone
import qualified Amazonka.Prelude as Prelude

-- | Contains a list of available options for a DB instance.
--
-- This data type is used as a response element in the
-- DescribeOrderableDBInstanceOptions action.
--
-- /See:/ 'newOrderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'
  { -- | The engine version of a DB instance.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Minimum provisioned IOPS per GiB for a DB instance.
    minIopsPerGib :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether a DB instance supports IAM database authentication.
    supportsIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | Minimum total provisioned IOPS for a DB instance.
    minIopsPerDbInstance :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether a DB instance is Multi-AZ capable.
    multiAZCapable :: Prelude.Maybe Prelude.Bool,
    -- | Maximum storage size for a DB instance.
    maxStorageSize :: Prelude.Maybe Prelude.Int,
    -- | The engine type of a DB instance.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Minimum storage size for a DB instance.
    minStorageSize :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether a DB instance supports provisioned IOPS.
    supportsIops :: Prelude.Maybe Prelude.Bool,
    -- | /(Not supported by Neptune)/
    supportsPerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The DB instance class for a DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The license model for a DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | A list of Availability Zones for a DB instance.
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | Indicates whether a DB instance supports encrypted storage.
    supportsStorageEncryption :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance can have a Read Replica.
    readReplicaCapable :: Prelude.Maybe Prelude.Bool,
    -- | Maximum provisioned IOPS per GiB for a DB instance.
    maxIopsPerGib :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether a DB instance is in a VPC.
    vpc :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a DB instance supports Enhanced Monitoring at
    -- intervals from 1 to 60 seconds.
    supportsEnhancedMonitoring :: Prelude.Maybe Prelude.Bool,
    -- | Maximum total provisioned IOPS for a DB instance.
    maxIopsPerDbInstance :: Prelude.Maybe Prelude.Int,
    -- | Indicates the storage type for a DB instance.
    storageType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrderableDBInstanceOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'orderableDBInstanceOption_engineVersion' - The engine version of a DB instance.
--
-- 'minIopsPerGib', 'orderableDBInstanceOption_minIopsPerGib' - Minimum provisioned IOPS per GiB for a DB instance.
--
-- 'supportsIAMDatabaseAuthentication', 'orderableDBInstanceOption_supportsIAMDatabaseAuthentication' - Indicates whether a DB instance supports IAM database authentication.
--
-- 'minIopsPerDbInstance', 'orderableDBInstanceOption_minIopsPerDbInstance' - Minimum total provisioned IOPS for a DB instance.
--
-- 'multiAZCapable', 'orderableDBInstanceOption_multiAZCapable' - Indicates whether a DB instance is Multi-AZ capable.
--
-- 'maxStorageSize', 'orderableDBInstanceOption_maxStorageSize' - Maximum storage size for a DB instance.
--
-- 'engine', 'orderableDBInstanceOption_engine' - The engine type of a DB instance.
--
-- 'minStorageSize', 'orderableDBInstanceOption_minStorageSize' - Minimum storage size for a DB instance.
--
-- 'supportsIops', 'orderableDBInstanceOption_supportsIops' - Indicates whether a DB instance supports provisioned IOPS.
--
-- 'supportsPerformanceInsights', 'orderableDBInstanceOption_supportsPerformanceInsights' - /(Not supported by Neptune)/
--
-- 'dbInstanceClass', 'orderableDBInstanceOption_dbInstanceClass' - The DB instance class for a DB instance.
--
-- 'licenseModel', 'orderableDBInstanceOption_licenseModel' - The license model for a DB instance.
--
-- 'availabilityZones', 'orderableDBInstanceOption_availabilityZones' - A list of Availability Zones for a DB instance.
--
-- 'supportsStorageEncryption', 'orderableDBInstanceOption_supportsStorageEncryption' - Indicates whether a DB instance supports encrypted storage.
--
-- 'readReplicaCapable', 'orderableDBInstanceOption_readReplicaCapable' - Indicates whether a DB instance can have a Read Replica.
--
-- 'maxIopsPerGib', 'orderableDBInstanceOption_maxIopsPerGib' - Maximum provisioned IOPS per GiB for a DB instance.
--
-- 'vpc', 'orderableDBInstanceOption_vpc' - Indicates whether a DB instance is in a VPC.
--
-- 'supportsEnhancedMonitoring', 'orderableDBInstanceOption_supportsEnhancedMonitoring' - Indicates whether a DB instance supports Enhanced Monitoring at
-- intervals from 1 to 60 seconds.
--
-- 'maxIopsPerDbInstance', 'orderableDBInstanceOption_maxIopsPerDbInstance' - Maximum total provisioned IOPS for a DB instance.
--
-- 'storageType', 'orderableDBInstanceOption_storageType' - Indicates the storage type for a DB instance.
newOrderableDBInstanceOption ::
  OrderableDBInstanceOption
newOrderableDBInstanceOption =
  OrderableDBInstanceOption'
    { engineVersion =
        Prelude.Nothing,
      minIopsPerGib = Prelude.Nothing,
      supportsIAMDatabaseAuthentication =
        Prelude.Nothing,
      minIopsPerDbInstance = Prelude.Nothing,
      multiAZCapable = Prelude.Nothing,
      maxStorageSize = Prelude.Nothing,
      engine = Prelude.Nothing,
      minStorageSize = Prelude.Nothing,
      supportsIops = Prelude.Nothing,
      supportsPerformanceInsights = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      supportsStorageEncryption = Prelude.Nothing,
      readReplicaCapable = Prelude.Nothing,
      maxIopsPerGib = Prelude.Nothing,
      vpc = Prelude.Nothing,
      supportsEnhancedMonitoring = Prelude.Nothing,
      maxIopsPerDbInstance = Prelude.Nothing,
      storageType = Prelude.Nothing
    }

-- | The engine version of a DB instance.
orderableDBInstanceOption_engineVersion :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_engineVersion = Lens.lens (\OrderableDBInstanceOption' {engineVersion} -> engineVersion) (\s@OrderableDBInstanceOption' {} a -> s {engineVersion = a} :: OrderableDBInstanceOption)

-- | Minimum provisioned IOPS per GiB for a DB instance.
orderableDBInstanceOption_minIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Double)
orderableDBInstanceOption_minIopsPerGib = Lens.lens (\OrderableDBInstanceOption' {minIopsPerGib} -> minIopsPerGib) (\s@OrderableDBInstanceOption' {} a -> s {minIopsPerGib = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports IAM database authentication.
orderableDBInstanceOption_supportsIAMDatabaseAuthentication :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsIAMDatabaseAuthentication = Lens.lens (\OrderableDBInstanceOption' {supportsIAMDatabaseAuthentication} -> supportsIAMDatabaseAuthentication) (\s@OrderableDBInstanceOption' {} a -> s {supportsIAMDatabaseAuthentication = a} :: OrderableDBInstanceOption)

-- | Minimum total provisioned IOPS for a DB instance.
orderableDBInstanceOption_minIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_minIopsPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {minIopsPerDbInstance} -> minIopsPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {minIopsPerDbInstance = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance is Multi-AZ capable.
orderableDBInstanceOption_multiAZCapable :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_multiAZCapable = Lens.lens (\OrderableDBInstanceOption' {multiAZCapable} -> multiAZCapable) (\s@OrderableDBInstanceOption' {} a -> s {multiAZCapable = a} :: OrderableDBInstanceOption)

-- | Maximum storage size for a DB instance.
orderableDBInstanceOption_maxStorageSize :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_maxStorageSize = Lens.lens (\OrderableDBInstanceOption' {maxStorageSize} -> maxStorageSize) (\s@OrderableDBInstanceOption' {} a -> s {maxStorageSize = a} :: OrderableDBInstanceOption)

-- | The engine type of a DB instance.
orderableDBInstanceOption_engine :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_engine = Lens.lens (\OrderableDBInstanceOption' {engine} -> engine) (\s@OrderableDBInstanceOption' {} a -> s {engine = a} :: OrderableDBInstanceOption)

-- | Minimum storage size for a DB instance.
orderableDBInstanceOption_minStorageSize :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_minStorageSize = Lens.lens (\OrderableDBInstanceOption' {minStorageSize} -> minStorageSize) (\s@OrderableDBInstanceOption' {} a -> s {minStorageSize = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports provisioned IOPS.
orderableDBInstanceOption_supportsIops :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsIops = Lens.lens (\OrderableDBInstanceOption' {supportsIops} -> supportsIops) (\s@OrderableDBInstanceOption' {} a -> s {supportsIops = a} :: OrderableDBInstanceOption)

-- | /(Not supported by Neptune)/
orderableDBInstanceOption_supportsPerformanceInsights :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsPerformanceInsights = Lens.lens (\OrderableDBInstanceOption' {supportsPerformanceInsights} -> supportsPerformanceInsights) (\s@OrderableDBInstanceOption' {} a -> s {supportsPerformanceInsights = a} :: OrderableDBInstanceOption)

-- | The DB instance class for a DB instance.
orderableDBInstanceOption_dbInstanceClass :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_dbInstanceClass = Lens.lens (\OrderableDBInstanceOption' {dbInstanceClass} -> dbInstanceClass) (\s@OrderableDBInstanceOption' {} a -> s {dbInstanceClass = a} :: OrderableDBInstanceOption)

-- | The license model for a DB instance.
orderableDBInstanceOption_licenseModel :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_licenseModel = Lens.lens (\OrderableDBInstanceOption' {licenseModel} -> licenseModel) (\s@OrderableDBInstanceOption' {} a -> s {licenseModel = a} :: OrderableDBInstanceOption)

-- | A list of Availability Zones for a DB instance.
orderableDBInstanceOption_availabilityZones :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe [AvailabilityZone])
orderableDBInstanceOption_availabilityZones = Lens.lens (\OrderableDBInstanceOption' {availabilityZones} -> availabilityZones) (\s@OrderableDBInstanceOption' {} a -> s {availabilityZones = a} :: OrderableDBInstanceOption) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether a DB instance supports encrypted storage.
orderableDBInstanceOption_supportsStorageEncryption :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsStorageEncryption = Lens.lens (\OrderableDBInstanceOption' {supportsStorageEncryption} -> supportsStorageEncryption) (\s@OrderableDBInstanceOption' {} a -> s {supportsStorageEncryption = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance can have a Read Replica.
orderableDBInstanceOption_readReplicaCapable :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_readReplicaCapable = Lens.lens (\OrderableDBInstanceOption' {readReplicaCapable} -> readReplicaCapable) (\s@OrderableDBInstanceOption' {} a -> s {readReplicaCapable = a} :: OrderableDBInstanceOption)

-- | Maximum provisioned IOPS per GiB for a DB instance.
orderableDBInstanceOption_maxIopsPerGib :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Double)
orderableDBInstanceOption_maxIopsPerGib = Lens.lens (\OrderableDBInstanceOption' {maxIopsPerGib} -> maxIopsPerGib) (\s@OrderableDBInstanceOption' {} a -> s {maxIopsPerGib = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance is in a VPC.
orderableDBInstanceOption_vpc :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_vpc = Lens.lens (\OrderableDBInstanceOption' {vpc} -> vpc) (\s@OrderableDBInstanceOption' {} a -> s {vpc = a} :: OrderableDBInstanceOption)

-- | Indicates whether a DB instance supports Enhanced Monitoring at
-- intervals from 1 to 60 seconds.
orderableDBInstanceOption_supportsEnhancedMonitoring :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Bool)
orderableDBInstanceOption_supportsEnhancedMonitoring = Lens.lens (\OrderableDBInstanceOption' {supportsEnhancedMonitoring} -> supportsEnhancedMonitoring) (\s@OrderableDBInstanceOption' {} a -> s {supportsEnhancedMonitoring = a} :: OrderableDBInstanceOption)

-- | Maximum total provisioned IOPS for a DB instance.
orderableDBInstanceOption_maxIopsPerDbInstance :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Int)
orderableDBInstanceOption_maxIopsPerDbInstance = Lens.lens (\OrderableDBInstanceOption' {maxIopsPerDbInstance} -> maxIopsPerDbInstance) (\s@OrderableDBInstanceOption' {} a -> s {maxIopsPerDbInstance = a} :: OrderableDBInstanceOption)

-- | Indicates the storage type for a DB instance.
orderableDBInstanceOption_storageType :: Lens.Lens' OrderableDBInstanceOption (Prelude.Maybe Prelude.Text)
orderableDBInstanceOption_storageType = Lens.lens (\OrderableDBInstanceOption' {storageType} -> storageType) (\s@OrderableDBInstanceOption' {} a -> s {storageType = a} :: OrderableDBInstanceOption)

instance Core.FromXML OrderableDBInstanceOption where
  parseXML x =
    OrderableDBInstanceOption'
      Prelude.<$> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "MinIopsPerGib")
      Prelude.<*> (x Core..@? "SupportsIAMDatabaseAuthentication")
      Prelude.<*> (x Core..@? "MinIopsPerDbInstance")
      Prelude.<*> (x Core..@? "MultiAZCapable")
      Prelude.<*> (x Core..@? "MaxStorageSize")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "MinStorageSize")
      Prelude.<*> (x Core..@? "SupportsIops")
      Prelude.<*> (x Core..@? "SupportsPerformanceInsights")
      Prelude.<*> (x Core..@? "DBInstanceClass")
      Prelude.<*> (x Core..@? "LicenseModel")
      Prelude.<*> ( x Core..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> (x Core..@? "SupportsStorageEncryption")
      Prelude.<*> (x Core..@? "ReadReplicaCapable")
      Prelude.<*> (x Core..@? "MaxIopsPerGib")
      Prelude.<*> (x Core..@? "Vpc")
      Prelude.<*> (x Core..@? "SupportsEnhancedMonitoring")
      Prelude.<*> (x Core..@? "MaxIopsPerDbInstance")
      Prelude.<*> (x Core..@? "StorageType")

instance Prelude.Hashable OrderableDBInstanceOption where
  hashWithSalt salt' OrderableDBInstanceOption' {..} =
    salt' `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` maxIopsPerDbInstance
      `Prelude.hashWithSalt` supportsEnhancedMonitoring
      `Prelude.hashWithSalt` vpc
      `Prelude.hashWithSalt` maxIopsPerGib
      `Prelude.hashWithSalt` readReplicaCapable
      `Prelude.hashWithSalt` supportsStorageEncryption
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` supportsPerformanceInsights
      `Prelude.hashWithSalt` supportsIops
      `Prelude.hashWithSalt` minStorageSize
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` maxStorageSize
      `Prelude.hashWithSalt` multiAZCapable
      `Prelude.hashWithSalt` minIopsPerDbInstance
      `Prelude.hashWithSalt` supportsIAMDatabaseAuthentication
      `Prelude.hashWithSalt` minIopsPerGib
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData OrderableDBInstanceOption where
  rnf OrderableDBInstanceOption' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf maxIopsPerDbInstance
      `Prelude.seq` Prelude.rnf supportsEnhancedMonitoring
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf maxIopsPerGib
      `Prelude.seq` Prelude.rnf readReplicaCapable
      `Prelude.seq` Prelude.rnf supportsStorageEncryption
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf licenseModel
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf supportsPerformanceInsights
      `Prelude.seq` Prelude.rnf supportsIops
      `Prelude.seq` Prelude.rnf minStorageSize
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf maxStorageSize
      `Prelude.seq` Prelude.rnf multiAZCapable
      `Prelude.seq` Prelude.rnf minIopsPerDbInstance
      `Prelude.seq` Prelude.rnf supportsIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf minIopsPerGib

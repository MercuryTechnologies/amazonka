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
-- Module      : Amazonka.EC2.Types.BlockDeviceMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.BlockDeviceMapping where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.EbsBlockDevice
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a block device mapping, which defines the EBS volumes and
-- instance store volumes to attach to an instance at launch.
--
-- /See:/ 'newBlockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { -- | The virtual device name (@ephemeral@N). Instance store volumes are
    -- numbered starting from 0. An instance type with 2 available instance
    -- store volumes can specify mappings for @ephemeral0@ and @ephemeral1@.
    -- The number of available instance store volumes depends on the instance
    -- type. After you connect to the instance, you must mount the volume.
    --
    -- NVMe instance store volumes are automatically enumerated and assigned a
    -- device name. Including them in your block device mapping has no effect.
    --
    -- Constraints: For M3 instances, you must specify instance store volumes
    -- in the block device mapping for the instance. When you launch an M3
    -- instance, we ignore any instance store volumes specified in the block
    -- device mapping for the AMI.
    virtualName :: Prelude.Maybe Prelude.Text,
    -- | To omit the device from the block device mapping, specify an empty
    -- string. When this property is specified, the device is removed from the
    -- block device mapping regardless of the assigned value.
    noDevice :: Prelude.Maybe Prelude.Text,
    -- | Parameters used to automatically set up EBS volumes when the instance is
    -- launched.
    ebs :: Prelude.Maybe EbsBlockDevice,
    -- | The device name (for example, @\/dev\/sdh@ or @xvdh@).
    deviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlockDeviceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualName', 'blockDeviceMapping_virtualName' - The virtual device name (@ephemeral@N). Instance store volumes are
-- numbered starting from 0. An instance type with 2 available instance
-- store volumes can specify mappings for @ephemeral0@ and @ephemeral1@.
-- The number of available instance store volumes depends on the instance
-- type. After you connect to the instance, you must mount the volume.
--
-- NVMe instance store volumes are automatically enumerated and assigned a
-- device name. Including them in your block device mapping has no effect.
--
-- Constraints: For M3 instances, you must specify instance store volumes
-- in the block device mapping for the instance. When you launch an M3
-- instance, we ignore any instance store volumes specified in the block
-- device mapping for the AMI.
--
-- 'noDevice', 'blockDeviceMapping_noDevice' - To omit the device from the block device mapping, specify an empty
-- string. When this property is specified, the device is removed from the
-- block device mapping regardless of the assigned value.
--
-- 'ebs', 'blockDeviceMapping_ebs' - Parameters used to automatically set up EBS volumes when the instance is
-- launched.
--
-- 'deviceName', 'blockDeviceMapping_deviceName' - The device name (for example, @\/dev\/sdh@ or @xvdh@).
newBlockDeviceMapping ::
  -- | 'deviceName'
  Prelude.Text ->
  BlockDeviceMapping
newBlockDeviceMapping pDeviceName_ =
  BlockDeviceMapping'
    { virtualName = Prelude.Nothing,
      noDevice = Prelude.Nothing,
      ebs = Prelude.Nothing,
      deviceName = pDeviceName_
    }

-- | The virtual device name (@ephemeral@N). Instance store volumes are
-- numbered starting from 0. An instance type with 2 available instance
-- store volumes can specify mappings for @ephemeral0@ and @ephemeral1@.
-- The number of available instance store volumes depends on the instance
-- type. After you connect to the instance, you must mount the volume.
--
-- NVMe instance store volumes are automatically enumerated and assigned a
-- device name. Including them in your block device mapping has no effect.
--
-- Constraints: For M3 instances, you must specify instance store volumes
-- in the block device mapping for the instance. When you launch an M3
-- instance, we ignore any instance store volumes specified in the block
-- device mapping for the AMI.
blockDeviceMapping_virtualName :: Lens.Lens' BlockDeviceMapping (Prelude.Maybe Prelude.Text)
blockDeviceMapping_virtualName = Lens.lens (\BlockDeviceMapping' {virtualName} -> virtualName) (\s@BlockDeviceMapping' {} a -> s {virtualName = a} :: BlockDeviceMapping)

-- | To omit the device from the block device mapping, specify an empty
-- string. When this property is specified, the device is removed from the
-- block device mapping regardless of the assigned value.
blockDeviceMapping_noDevice :: Lens.Lens' BlockDeviceMapping (Prelude.Maybe Prelude.Text)
blockDeviceMapping_noDevice = Lens.lens (\BlockDeviceMapping' {noDevice} -> noDevice) (\s@BlockDeviceMapping' {} a -> s {noDevice = a} :: BlockDeviceMapping)

-- | Parameters used to automatically set up EBS volumes when the instance is
-- launched.
blockDeviceMapping_ebs :: Lens.Lens' BlockDeviceMapping (Prelude.Maybe EbsBlockDevice)
blockDeviceMapping_ebs = Lens.lens (\BlockDeviceMapping' {ebs} -> ebs) (\s@BlockDeviceMapping' {} a -> s {ebs = a} :: BlockDeviceMapping)

-- | The device name (for example, @\/dev\/sdh@ or @xvdh@).
blockDeviceMapping_deviceName :: Lens.Lens' BlockDeviceMapping Prelude.Text
blockDeviceMapping_deviceName = Lens.lens (\BlockDeviceMapping' {deviceName} -> deviceName) (\s@BlockDeviceMapping' {} a -> s {deviceName = a} :: BlockDeviceMapping)

instance Core.FromXML BlockDeviceMapping where
  parseXML x =
    BlockDeviceMapping'
      Prelude.<$> (x Core..@? "virtualName")
      Prelude.<*> (x Core..@? "noDevice")
      Prelude.<*> (x Core..@? "ebs")
      Prelude.<*> (x Core..@ "deviceName")

instance Prelude.Hashable BlockDeviceMapping where
  hashWithSalt salt' BlockDeviceMapping' {..} =
    salt' `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` ebs
      `Prelude.hashWithSalt` noDevice
      `Prelude.hashWithSalt` virtualName

instance Prelude.NFData BlockDeviceMapping where
  rnf BlockDeviceMapping' {..} =
    Prelude.rnf virtualName
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf ebs
      `Prelude.seq` Prelude.rnf noDevice

instance Core.ToQuery BlockDeviceMapping where
  toQuery BlockDeviceMapping' {..} =
    Prelude.mconcat
      [ "VirtualName" Core.=: virtualName,
        "NoDevice" Core.=: noDevice,
        "Ebs" Core.=: ebs,
        "DeviceName" Core.=: deviceName
      ]

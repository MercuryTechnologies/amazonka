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
-- Module      : Amazonka.StorageGateway.Types.VolumeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.VolumeInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a storage volume object.
--
-- /See:/ 'newVolumeInfo' smart constructor.
data VolumeInfo = VolumeInfo'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | One of the VolumeStatus values that indicates the state of the storage
    -- volume.
    volumeAttachmentStatus :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the storage volume. For example, the
    -- following is a valid ARN:
    --
    -- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/volume\/vol-1122AABB@
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
    -- hyphens (-).
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The size of the volume in bytes.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
    -- hyphens (-).
    volumeSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier assigned to the volume. This ID becomes part of
    -- the volume Amazon Resource Name (ARN), which you use as input for other
    -- operations.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
    -- hyphens (-).
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier assigned to your gateway during activation. This
    -- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
    -- as input for other operations.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
    -- hyphens (-).
    gatewayId :: Prelude.Maybe Prelude.Text,
    -- | One of the VolumeType enumeration values describing the type of the
    -- volume.
    volumeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'volumeInfo_gatewayARN' - Undocumented member.
--
-- 'volumeAttachmentStatus', 'volumeInfo_volumeAttachmentStatus' - One of the VolumeStatus values that indicates the state of the storage
-- volume.
--
-- 'volumeARN', 'volumeInfo_volumeARN' - The Amazon Resource Name (ARN) for the storage volume. For example, the
-- following is a valid ARN:
--
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/volume\/vol-1122AABB@
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
--
-- 'volumeSizeInBytes', 'volumeInfo_volumeSizeInBytes' - The size of the volume in bytes.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
--
-- 'volumeId', 'volumeInfo_volumeId' - The unique identifier assigned to the volume. This ID becomes part of
-- the volume Amazon Resource Name (ARN), which you use as input for other
-- operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
--
-- 'gatewayId', 'volumeInfo_gatewayId' - The unique identifier assigned to your gateway during activation. This
-- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
-- as input for other operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
--
-- 'volumeType', 'volumeInfo_volumeType' - One of the VolumeType enumeration values describing the type of the
-- volume.
newVolumeInfo ::
  VolumeInfo
newVolumeInfo =
  VolumeInfo'
    { gatewayARN = Prelude.Nothing,
      volumeAttachmentStatus = Prelude.Nothing,
      volumeARN = Prelude.Nothing,
      volumeSizeInBytes = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      gatewayId = Prelude.Nothing,
      volumeType = Prelude.Nothing
    }

-- | Undocumented member.
volumeInfo_gatewayARN :: Lens.Lens' VolumeInfo (Prelude.Maybe Prelude.Text)
volumeInfo_gatewayARN = Lens.lens (\VolumeInfo' {gatewayARN} -> gatewayARN) (\s@VolumeInfo' {} a -> s {gatewayARN = a} :: VolumeInfo)

-- | One of the VolumeStatus values that indicates the state of the storage
-- volume.
volumeInfo_volumeAttachmentStatus :: Lens.Lens' VolumeInfo (Prelude.Maybe Prelude.Text)
volumeInfo_volumeAttachmentStatus = Lens.lens (\VolumeInfo' {volumeAttachmentStatus} -> volumeAttachmentStatus) (\s@VolumeInfo' {} a -> s {volumeAttachmentStatus = a} :: VolumeInfo)

-- | The Amazon Resource Name (ARN) for the storage volume. For example, the
-- following is a valid ARN:
--
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/volume\/vol-1122AABB@
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
volumeInfo_volumeARN :: Lens.Lens' VolumeInfo (Prelude.Maybe Prelude.Text)
volumeInfo_volumeARN = Lens.lens (\VolumeInfo' {volumeARN} -> volumeARN) (\s@VolumeInfo' {} a -> s {volumeARN = a} :: VolumeInfo)

-- | The size of the volume in bytes.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
volumeInfo_volumeSizeInBytes :: Lens.Lens' VolumeInfo (Prelude.Maybe Prelude.Integer)
volumeInfo_volumeSizeInBytes = Lens.lens (\VolumeInfo' {volumeSizeInBytes} -> volumeSizeInBytes) (\s@VolumeInfo' {} a -> s {volumeSizeInBytes = a} :: VolumeInfo)

-- | The unique identifier assigned to the volume. This ID becomes part of
-- the volume Amazon Resource Name (ARN), which you use as input for other
-- operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
volumeInfo_volumeId :: Lens.Lens' VolumeInfo (Prelude.Maybe Prelude.Text)
volumeInfo_volumeId = Lens.lens (\VolumeInfo' {volumeId} -> volumeId) (\s@VolumeInfo' {} a -> s {volumeId = a} :: VolumeInfo)

-- | The unique identifier assigned to your gateway during activation. This
-- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
-- as input for other operations.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
volumeInfo_gatewayId :: Lens.Lens' VolumeInfo (Prelude.Maybe Prelude.Text)
volumeInfo_gatewayId = Lens.lens (\VolumeInfo' {gatewayId} -> gatewayId) (\s@VolumeInfo' {} a -> s {gatewayId = a} :: VolumeInfo)

-- | One of the VolumeType enumeration values describing the type of the
-- volume.
volumeInfo_volumeType :: Lens.Lens' VolumeInfo (Prelude.Maybe Prelude.Text)
volumeInfo_volumeType = Lens.lens (\VolumeInfo' {volumeType} -> volumeType) (\s@VolumeInfo' {} a -> s {volumeType = a} :: VolumeInfo)

instance Core.FromJSON VolumeInfo where
  parseJSON =
    Core.withObject
      "VolumeInfo"
      ( \x ->
          VolumeInfo'
            Prelude.<$> (x Core..:? "GatewayARN")
            Prelude.<*> (x Core..:? "VolumeAttachmentStatus")
            Prelude.<*> (x Core..:? "VolumeARN")
            Prelude.<*> (x Core..:? "VolumeSizeInBytes")
            Prelude.<*> (x Core..:? "VolumeId")
            Prelude.<*> (x Core..:? "GatewayId")
            Prelude.<*> (x Core..:? "VolumeType")
      )

instance Prelude.Hashable VolumeInfo where
  hashWithSalt salt' VolumeInfo' {..} =
    salt' `Prelude.hashWithSalt` volumeType
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` volumeId
      `Prelude.hashWithSalt` volumeSizeInBytes
      `Prelude.hashWithSalt` volumeARN
      `Prelude.hashWithSalt` volumeAttachmentStatus
      `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData VolumeInfo where
  rnf VolumeInfo' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf volumeType
      `Prelude.seq` Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf volumeId
      `Prelude.seq` Prelude.rnf volumeSizeInBytes
      `Prelude.seq` Prelude.rnf volumeARN
      `Prelude.seq` Prelude.rnf volumeAttachmentStatus

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
-- Module      : Amazonka.EC2.Types.VolumeStatusItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VolumeStatusItem where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VolumeStatusAction
import Amazonka.EC2.Types.VolumeStatusAttachmentStatus
import Amazonka.EC2.Types.VolumeStatusEvent
import Amazonka.EC2.Types.VolumeStatusInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the volume status.
--
-- /See:/ 'newVolumeStatusItem' smart constructor.
data VolumeStatusItem = VolumeStatusItem'
  { -- | The volume status.
    volumeStatus :: Prelude.Maybe VolumeStatusInfo,
    -- | The details of the operation.
    actions :: Prelude.Maybe [VolumeStatusAction],
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | A list of events associated with the volume.
    events :: Prelude.Maybe [VolumeStatusEvent],
    -- | The Availability Zone of the volume.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The volume ID.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | Information about the instances to which the volume is attached.
    attachmentStatuses :: Prelude.Maybe [VolumeStatusAttachmentStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeStatusItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeStatus', 'volumeStatusItem_volumeStatus' - The volume status.
--
-- 'actions', 'volumeStatusItem_actions' - The details of the operation.
--
-- 'outpostArn', 'volumeStatusItem_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'events', 'volumeStatusItem_events' - A list of events associated with the volume.
--
-- 'availabilityZone', 'volumeStatusItem_availabilityZone' - The Availability Zone of the volume.
--
-- 'volumeId', 'volumeStatusItem_volumeId' - The volume ID.
--
-- 'attachmentStatuses', 'volumeStatusItem_attachmentStatuses' - Information about the instances to which the volume is attached.
newVolumeStatusItem ::
  VolumeStatusItem
newVolumeStatusItem =
  VolumeStatusItem'
    { volumeStatus = Prelude.Nothing,
      actions = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      events = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      attachmentStatuses = Prelude.Nothing
    }

-- | The volume status.
volumeStatusItem_volumeStatus :: Lens.Lens' VolumeStatusItem (Prelude.Maybe VolumeStatusInfo)
volumeStatusItem_volumeStatus = Lens.lens (\VolumeStatusItem' {volumeStatus} -> volumeStatus) (\s@VolumeStatusItem' {} a -> s {volumeStatus = a} :: VolumeStatusItem)

-- | The details of the operation.
volumeStatusItem_actions :: Lens.Lens' VolumeStatusItem (Prelude.Maybe [VolumeStatusAction])
volumeStatusItem_actions = Lens.lens (\VolumeStatusItem' {actions} -> actions) (\s@VolumeStatusItem' {} a -> s {actions = a} :: VolumeStatusItem) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Outpost.
volumeStatusItem_outpostArn :: Lens.Lens' VolumeStatusItem (Prelude.Maybe Prelude.Text)
volumeStatusItem_outpostArn = Lens.lens (\VolumeStatusItem' {outpostArn} -> outpostArn) (\s@VolumeStatusItem' {} a -> s {outpostArn = a} :: VolumeStatusItem)

-- | A list of events associated with the volume.
volumeStatusItem_events :: Lens.Lens' VolumeStatusItem (Prelude.Maybe [VolumeStatusEvent])
volumeStatusItem_events = Lens.lens (\VolumeStatusItem' {events} -> events) (\s@VolumeStatusItem' {} a -> s {events = a} :: VolumeStatusItem) Prelude.. Lens.mapping Lens.coerced

-- | The Availability Zone of the volume.
volumeStatusItem_availabilityZone :: Lens.Lens' VolumeStatusItem (Prelude.Maybe Prelude.Text)
volumeStatusItem_availabilityZone = Lens.lens (\VolumeStatusItem' {availabilityZone} -> availabilityZone) (\s@VolumeStatusItem' {} a -> s {availabilityZone = a} :: VolumeStatusItem)

-- | The volume ID.
volumeStatusItem_volumeId :: Lens.Lens' VolumeStatusItem (Prelude.Maybe Prelude.Text)
volumeStatusItem_volumeId = Lens.lens (\VolumeStatusItem' {volumeId} -> volumeId) (\s@VolumeStatusItem' {} a -> s {volumeId = a} :: VolumeStatusItem)

-- | Information about the instances to which the volume is attached.
volumeStatusItem_attachmentStatuses :: Lens.Lens' VolumeStatusItem (Prelude.Maybe [VolumeStatusAttachmentStatus])
volumeStatusItem_attachmentStatuses = Lens.lens (\VolumeStatusItem' {attachmentStatuses} -> attachmentStatuses) (\s@VolumeStatusItem' {} a -> s {attachmentStatuses = a} :: VolumeStatusItem) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML VolumeStatusItem where
  parseXML x =
    VolumeStatusItem'
      Prelude.<$> (x Core..@? "volumeStatus")
      Prelude.<*> ( x Core..@? "actionsSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "outpostArn")
      Prelude.<*> ( x Core..@? "eventsSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "availabilityZone")
      Prelude.<*> (x Core..@? "volumeId")
      Prelude.<*> ( x Core..@? "attachmentStatuses"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable VolumeStatusItem where
  hashWithSalt salt' VolumeStatusItem' {..} =
    salt' `Prelude.hashWithSalt` attachmentStatuses
      `Prelude.hashWithSalt` volumeId
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` volumeStatus

instance Prelude.NFData VolumeStatusItem where
  rnf VolumeStatusItem' {..} =
    Prelude.rnf volumeStatus
      `Prelude.seq` Prelude.rnf attachmentStatuses
      `Prelude.seq` Prelude.rnf volumeId
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf events
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf actions

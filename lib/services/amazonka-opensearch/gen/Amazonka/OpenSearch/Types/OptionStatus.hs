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
-- Module      : Amazonka.OpenSearch.Types.OptionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.OptionStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.OptionState
import qualified Amazonka.Prelude as Prelude

-- | Provides the current status of the entity.
--
-- /See:/ 'newOptionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { -- | Indicates whether the domain is being deleted.
    pendingDeletion :: Prelude.Maybe Prelude.Bool,
    -- | The latest version of the entity.
    updateVersion :: Prelude.Maybe Prelude.Natural,
    -- | The timestamp of when the entity was created.
    creationDate :: Core.POSIX,
    -- | The timestamp of the last time the entity was updated.
    updateDate :: Core.POSIX,
    -- | Provides the @OptionState@ for the domain.
    state :: OptionState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pendingDeletion', 'optionStatus_pendingDeletion' - Indicates whether the domain is being deleted.
--
-- 'updateVersion', 'optionStatus_updateVersion' - The latest version of the entity.
--
-- 'creationDate', 'optionStatus_creationDate' - The timestamp of when the entity was created.
--
-- 'updateDate', 'optionStatus_updateDate' - The timestamp of the last time the entity was updated.
--
-- 'state', 'optionStatus_state' - Provides the @OptionState@ for the domain.
newOptionStatus ::
  -- | 'creationDate'
  Prelude.UTCTime ->
  -- | 'updateDate'
  Prelude.UTCTime ->
  -- | 'state'
  OptionState ->
  OptionStatus
newOptionStatus pCreationDate_ pUpdateDate_ pState_ =
  OptionStatus'
    { pendingDeletion = Prelude.Nothing,
      updateVersion = Prelude.Nothing,
      creationDate = Core._Time Lens.# pCreationDate_,
      updateDate = Core._Time Lens.# pUpdateDate_,
      state = pState_
    }

-- | Indicates whether the domain is being deleted.
optionStatus_pendingDeletion :: Lens.Lens' OptionStatus (Prelude.Maybe Prelude.Bool)
optionStatus_pendingDeletion = Lens.lens (\OptionStatus' {pendingDeletion} -> pendingDeletion) (\s@OptionStatus' {} a -> s {pendingDeletion = a} :: OptionStatus)

-- | The latest version of the entity.
optionStatus_updateVersion :: Lens.Lens' OptionStatus (Prelude.Maybe Prelude.Natural)
optionStatus_updateVersion = Lens.lens (\OptionStatus' {updateVersion} -> updateVersion) (\s@OptionStatus' {} a -> s {updateVersion = a} :: OptionStatus)

-- | The timestamp of when the entity was created.
optionStatus_creationDate :: Lens.Lens' OptionStatus Prelude.UTCTime
optionStatus_creationDate = Lens.lens (\OptionStatus' {creationDate} -> creationDate) (\s@OptionStatus' {} a -> s {creationDate = a} :: OptionStatus) Prelude.. Core._Time

-- | The timestamp of the last time the entity was updated.
optionStatus_updateDate :: Lens.Lens' OptionStatus Prelude.UTCTime
optionStatus_updateDate = Lens.lens (\OptionStatus' {updateDate} -> updateDate) (\s@OptionStatus' {} a -> s {updateDate = a} :: OptionStatus) Prelude.. Core._Time

-- | Provides the @OptionState@ for the domain.
optionStatus_state :: Lens.Lens' OptionStatus OptionState
optionStatus_state = Lens.lens (\OptionStatus' {state} -> state) (\s@OptionStatus' {} a -> s {state = a} :: OptionStatus)

instance Core.FromJSON OptionStatus where
  parseJSON =
    Core.withObject
      "OptionStatus"
      ( \x ->
          OptionStatus'
            Prelude.<$> (x Core..:? "PendingDeletion")
            Prelude.<*> (x Core..:? "UpdateVersion")
            Prelude.<*> (x Core..: "CreationDate")
            Prelude.<*> (x Core..: "UpdateDate")
            Prelude.<*> (x Core..: "State")
      )

instance Prelude.Hashable OptionStatus where
  hashWithSalt salt' OptionStatus' {..} =
    salt' `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` updateDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` updateVersion
      `Prelude.hashWithSalt` pendingDeletion

instance Prelude.NFData OptionStatus where
  rnf OptionStatus' {..} =
    Prelude.rnf pendingDeletion
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf updateDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf updateVersion

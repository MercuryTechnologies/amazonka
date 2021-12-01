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
-- Module      : Amazonka.AMP.Types.AlertManagerDefinitionDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.AlertManagerDefinitionDescription where

import Amazonka.AMP.Types.AlertManagerDefinitionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of an alert manager definition.
--
-- /See:/ 'newAlertManagerDefinitionDescription' smart constructor.
data AlertManagerDefinitionDescription = AlertManagerDefinitionDescription'
  { -- | The time when the alert manager definition was created.
    createdAt :: Core.POSIX,
    -- | The alert manager definition.
    data' :: Core.Base64,
    -- | The time when the alert manager definition was modified.
    modifiedAt :: Core.POSIX,
    -- | The status of alert manager definition.
    status :: AlertManagerDefinitionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlertManagerDefinitionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'alertManagerDefinitionDescription_createdAt' - The time when the alert manager definition was created.
--
-- 'data'', 'alertManagerDefinitionDescription_data' - The alert manager definition.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'modifiedAt', 'alertManagerDefinitionDescription_modifiedAt' - The time when the alert manager definition was modified.
--
-- 'status', 'alertManagerDefinitionDescription_status' - The status of alert manager definition.
newAlertManagerDefinitionDescription ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'data''
  Prelude.ByteString ->
  -- | 'modifiedAt'
  Prelude.UTCTime ->
  -- | 'status'
  AlertManagerDefinitionStatus ->
  AlertManagerDefinitionDescription
newAlertManagerDefinitionDescription
  pCreatedAt_
  pData_
  pModifiedAt_
  pStatus_ =
    AlertManagerDefinitionDescription'
      { createdAt =
          Core._Time Lens.# pCreatedAt_,
        data' = Core._Base64 Lens.# pData_,
        modifiedAt =
          Core._Time Lens.# pModifiedAt_,
        status = pStatus_
      }

-- | The time when the alert manager definition was created.
alertManagerDefinitionDescription_createdAt :: Lens.Lens' AlertManagerDefinitionDescription Prelude.UTCTime
alertManagerDefinitionDescription_createdAt = Lens.lens (\AlertManagerDefinitionDescription' {createdAt} -> createdAt) (\s@AlertManagerDefinitionDescription' {} a -> s {createdAt = a} :: AlertManagerDefinitionDescription) Prelude.. Core._Time

-- | The alert manager definition.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
alertManagerDefinitionDescription_data :: Lens.Lens' AlertManagerDefinitionDescription Prelude.ByteString
alertManagerDefinitionDescription_data = Lens.lens (\AlertManagerDefinitionDescription' {data'} -> data') (\s@AlertManagerDefinitionDescription' {} a -> s {data' = a} :: AlertManagerDefinitionDescription) Prelude.. Core._Base64

-- | The time when the alert manager definition was modified.
alertManagerDefinitionDescription_modifiedAt :: Lens.Lens' AlertManagerDefinitionDescription Prelude.UTCTime
alertManagerDefinitionDescription_modifiedAt = Lens.lens (\AlertManagerDefinitionDescription' {modifiedAt} -> modifiedAt) (\s@AlertManagerDefinitionDescription' {} a -> s {modifiedAt = a} :: AlertManagerDefinitionDescription) Prelude.. Core._Time

-- | The status of alert manager definition.
alertManagerDefinitionDescription_status :: Lens.Lens' AlertManagerDefinitionDescription AlertManagerDefinitionStatus
alertManagerDefinitionDescription_status = Lens.lens (\AlertManagerDefinitionDescription' {status} -> status) (\s@AlertManagerDefinitionDescription' {} a -> s {status = a} :: AlertManagerDefinitionDescription)

instance
  Core.FromJSON
    AlertManagerDefinitionDescription
  where
  parseJSON =
    Core.withObject
      "AlertManagerDefinitionDescription"
      ( \x ->
          AlertManagerDefinitionDescription'
            Prelude.<$> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "data")
            Prelude.<*> (x Core..: "modifiedAt")
            Prelude.<*> (x Core..: "status")
      )

instance
  Prelude.Hashable
    AlertManagerDefinitionDescription
  where
  hashWithSalt
    salt'
    AlertManagerDefinitionDescription' {..} =
      salt' `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` modifiedAt
        `Prelude.hashWithSalt` data'
        `Prelude.hashWithSalt` createdAt

instance
  Prelude.NFData
    AlertManagerDefinitionDescription
  where
  rnf AlertManagerDefinitionDescription' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf modifiedAt
      `Prelude.seq` Prelude.rnf data'

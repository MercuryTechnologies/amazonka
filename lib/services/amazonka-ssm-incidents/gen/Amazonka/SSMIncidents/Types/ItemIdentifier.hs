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
-- Module      : Amazonka.SSMIncidents.Types.ItemIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.ItemIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.ItemType
import Amazonka.SSMIncidents.Types.ItemValue

-- | Details and type of a related item.
--
-- /See:/ 'newItemIdentifier' smart constructor.
data ItemIdentifier = ItemIdentifier'
  { -- | The type of related item. Incident Manager supports the following types:
    --
    -- -   @ANALYSIS@
    --
    -- -   @INCIDENT@
    --
    -- -   @METRIC@
    --
    -- -   @PARENT@
    --
    -- -   @ATTACHMENT@
    --
    -- -   @OTHER@
    type' :: ItemType,
    -- | Details about the related item.
    value :: ItemValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ItemIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'itemIdentifier_type' - The type of related item. Incident Manager supports the following types:
--
-- -   @ANALYSIS@
--
-- -   @INCIDENT@
--
-- -   @METRIC@
--
-- -   @PARENT@
--
-- -   @ATTACHMENT@
--
-- -   @OTHER@
--
-- 'value', 'itemIdentifier_value' - Details about the related item.
newItemIdentifier ::
  -- | 'type''
  ItemType ->
  -- | 'value'
  ItemValue ->
  ItemIdentifier
newItemIdentifier pType_ pValue_ =
  ItemIdentifier' {type' = pType_, value = pValue_}

-- | The type of related item. Incident Manager supports the following types:
--
-- -   @ANALYSIS@
--
-- -   @INCIDENT@
--
-- -   @METRIC@
--
-- -   @PARENT@
--
-- -   @ATTACHMENT@
--
-- -   @OTHER@
itemIdentifier_type :: Lens.Lens' ItemIdentifier ItemType
itemIdentifier_type = Lens.lens (\ItemIdentifier' {type'} -> type') (\s@ItemIdentifier' {} a -> s {type' = a} :: ItemIdentifier)

-- | Details about the related item.
itemIdentifier_value :: Lens.Lens' ItemIdentifier ItemValue
itemIdentifier_value = Lens.lens (\ItemIdentifier' {value} -> value) (\s@ItemIdentifier' {} a -> s {value = a} :: ItemIdentifier)

instance Core.FromJSON ItemIdentifier where
  parseJSON =
    Core.withObject
      "ItemIdentifier"
      ( \x ->
          ItemIdentifier'
            Prelude.<$> (x Core..: "type") Prelude.<*> (x Core..: "value")
      )

instance Prelude.Hashable ItemIdentifier where
  hashWithSalt salt' ItemIdentifier' {..} =
    salt' `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ItemIdentifier where
  rnf ItemIdentifier' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Core.ToJSON ItemIdentifier where
  toJSON ItemIdentifier' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("type" Core..= type'),
            Prelude.Just ("value" Core..= value)
          ]
      )

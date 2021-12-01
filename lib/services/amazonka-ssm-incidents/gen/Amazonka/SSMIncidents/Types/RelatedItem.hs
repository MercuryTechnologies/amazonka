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
-- Module      : Amazonka.SSMIncidents.Types.RelatedItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.RelatedItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.ItemIdentifier

-- | Resources that responders use to triage and mitigate the incident.
--
-- /See:/ 'newRelatedItem' smart constructor.
data RelatedItem = RelatedItem'
  { -- | The title of the related item.
    title :: Prelude.Maybe Prelude.Text,
    -- | Details about the related item.
    identifier :: ItemIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelatedItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'title', 'relatedItem_title' - The title of the related item.
--
-- 'identifier', 'relatedItem_identifier' - Details about the related item.
newRelatedItem ::
  -- | 'identifier'
  ItemIdentifier ->
  RelatedItem
newRelatedItem pIdentifier_ =
  RelatedItem'
    { title = Prelude.Nothing,
      identifier = pIdentifier_
    }

-- | The title of the related item.
relatedItem_title :: Lens.Lens' RelatedItem (Prelude.Maybe Prelude.Text)
relatedItem_title = Lens.lens (\RelatedItem' {title} -> title) (\s@RelatedItem' {} a -> s {title = a} :: RelatedItem)

-- | Details about the related item.
relatedItem_identifier :: Lens.Lens' RelatedItem ItemIdentifier
relatedItem_identifier = Lens.lens (\RelatedItem' {identifier} -> identifier) (\s@RelatedItem' {} a -> s {identifier = a} :: RelatedItem)

instance Core.FromJSON RelatedItem where
  parseJSON =
    Core.withObject
      "RelatedItem"
      ( \x ->
          RelatedItem'
            Prelude.<$> (x Core..:? "title")
            Prelude.<*> (x Core..: "identifier")
      )

instance Prelude.Hashable RelatedItem where
  hashWithSalt salt' RelatedItem' {..} =
    salt' `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` title

instance Prelude.NFData RelatedItem where
  rnf RelatedItem' {..} =
    Prelude.rnf title
      `Prelude.seq` Prelude.rnf identifier

instance Core.ToJSON RelatedItem where
  toJSON RelatedItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("title" Core..=) Prelude.<$> title,
            Prelude.Just ("identifier" Core..= identifier)
          ]
      )

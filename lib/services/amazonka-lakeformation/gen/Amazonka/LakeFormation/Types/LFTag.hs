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
-- Module      : Amazonka.LakeFormation.Types.LFTag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.LFTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure that allows an admin to grant user permissions on certain
-- conditions. For example, granting a role access to all columns not
-- tagged \'PII\' of tables tagged \'Prod\'.
--
-- /See:/ 'newLFTag' smart constructor.
data LFTag = LFTag'
  { -- | The key-name for the tag.
    tagKey :: Prelude.Text,
    -- | A list of possible values an attribute can take.
    tagValues :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LFTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKey', 'lFTag_tagKey' - The key-name for the tag.
--
-- 'tagValues', 'lFTag_tagValues' - A list of possible values an attribute can take.
newLFTag ::
  -- | 'tagKey'
  Prelude.Text ->
  -- | 'tagValues'
  Prelude.NonEmpty Prelude.Text ->
  LFTag
newLFTag pTagKey_ pTagValues_ =
  LFTag'
    { tagKey = pTagKey_,
      tagValues = Lens.coerced Lens.# pTagValues_
    }

-- | The key-name for the tag.
lFTag_tagKey :: Lens.Lens' LFTag Prelude.Text
lFTag_tagKey = Lens.lens (\LFTag' {tagKey} -> tagKey) (\s@LFTag' {} a -> s {tagKey = a} :: LFTag)

-- | A list of possible values an attribute can take.
lFTag_tagValues :: Lens.Lens' LFTag (Prelude.NonEmpty Prelude.Text)
lFTag_tagValues = Lens.lens (\LFTag' {tagValues} -> tagValues) (\s@LFTag' {} a -> s {tagValues = a} :: LFTag) Prelude.. Lens.coerced

instance Core.FromJSON LFTag where
  parseJSON =
    Core.withObject
      "LFTag"
      ( \x ->
          LFTag'
            Prelude.<$> (x Core..: "TagKey")
            Prelude.<*> (x Core..: "TagValues")
      )

instance Prelude.Hashable LFTag where
  hashWithSalt salt' LFTag' {..} =
    salt' `Prelude.hashWithSalt` tagValues
      `Prelude.hashWithSalt` tagKey

instance Prelude.NFData LFTag where
  rnf LFTag' {..} =
    Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf tagValues

instance Core.ToJSON LFTag where
  toJSON LFTag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TagKey" Core..= tagKey),
            Prelude.Just ("TagValues" Core..= tagValues)
          ]
      )

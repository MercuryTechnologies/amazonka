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
-- Module      : Amazonka.MacieV2.Types.KeyValuePair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.KeyValuePair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the tags that are associated with an S3
-- bucket or object. Each tag consists of a required tag key and an
-- associated tag value.
--
-- /See:/ 'newKeyValuePair' smart constructor.
data KeyValuePair = KeyValuePair'
  { -- | One part of a key-value pair that comprises a tag. A tag value acts as a
    -- descriptor for a tag key. A tag value can be an empty string.
    value :: Prelude.Maybe Prelude.Text,
    -- | One part of a key-value pair that comprises a tag. A tag key is a
    -- general label that acts as a category for more specific tag values.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyValuePair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'keyValuePair_value' - One part of a key-value pair that comprises a tag. A tag value acts as a
-- descriptor for a tag key. A tag value can be an empty string.
--
-- 'key', 'keyValuePair_key' - One part of a key-value pair that comprises a tag. A tag key is a
-- general label that acts as a category for more specific tag values.
newKeyValuePair ::
  KeyValuePair
newKeyValuePair =
  KeyValuePair'
    { value = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | One part of a key-value pair that comprises a tag. A tag value acts as a
-- descriptor for a tag key. A tag value can be an empty string.
keyValuePair_value :: Lens.Lens' KeyValuePair (Prelude.Maybe Prelude.Text)
keyValuePair_value = Lens.lens (\KeyValuePair' {value} -> value) (\s@KeyValuePair' {} a -> s {value = a} :: KeyValuePair)

-- | One part of a key-value pair that comprises a tag. A tag key is a
-- general label that acts as a category for more specific tag values.
keyValuePair_key :: Lens.Lens' KeyValuePair (Prelude.Maybe Prelude.Text)
keyValuePair_key = Lens.lens (\KeyValuePair' {key} -> key) (\s@KeyValuePair' {} a -> s {key = a} :: KeyValuePair)

instance Core.FromJSON KeyValuePair where
  parseJSON =
    Core.withObject
      "KeyValuePair"
      ( \x ->
          KeyValuePair'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "key")
      )

instance Prelude.Hashable KeyValuePair where
  hashWithSalt salt' KeyValuePair' {..} =
    salt' `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData KeyValuePair where
  rnf KeyValuePair' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key

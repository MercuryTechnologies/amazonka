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
-- Module      : Amazonka.KMS.Types.ListGrantsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.ListGrantsResponse where

import qualified Amazonka.Core as Core
import Amazonka.KMS.Types.GrantListEntry
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newListGrantsResponse' smart constructor.
data ListGrantsResponse = ListGrantsResponse'
  { -- | A flag that indicates whether there are more items in the list. When
    -- this value is true, the list in this response is truncated. To get more
    -- items, pass the value of the @NextMarker@ element in thisresponse to the
    -- @Marker@ parameter in a subsequent request.
    truncated :: Prelude.Maybe Prelude.Bool,
    -- | A list of grants.
    grants :: Prelude.Maybe [GrantListEntry],
    -- | When @Truncated@ is true, this element is present and contains the value
    -- to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGrantsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'truncated', 'listGrantsResponse_truncated' - A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
--
-- 'grants', 'listGrantsResponse_grants' - A list of grants.
--
-- 'nextMarker', 'listGrantsResponse_nextMarker' - When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
newListGrantsResponse ::
  ListGrantsResponse
newListGrantsResponse =
  ListGrantsResponse'
    { truncated = Prelude.Nothing,
      grants = Prelude.Nothing,
      nextMarker = Prelude.Nothing
    }

-- | A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
listGrantsResponse_truncated :: Lens.Lens' ListGrantsResponse (Prelude.Maybe Prelude.Bool)
listGrantsResponse_truncated = Lens.lens (\ListGrantsResponse' {truncated} -> truncated) (\s@ListGrantsResponse' {} a -> s {truncated = a} :: ListGrantsResponse)

-- | A list of grants.
listGrantsResponse_grants :: Lens.Lens' ListGrantsResponse (Prelude.Maybe [GrantListEntry])
listGrantsResponse_grants = Lens.lens (\ListGrantsResponse' {grants} -> grants) (\s@ListGrantsResponse' {} a -> s {grants = a} :: ListGrantsResponse) Prelude.. Lens.mapping Lens.coerced

-- | When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
listGrantsResponse_nextMarker :: Lens.Lens' ListGrantsResponse (Prelude.Maybe Prelude.Text)
listGrantsResponse_nextMarker = Lens.lens (\ListGrantsResponse' {nextMarker} -> nextMarker) (\s@ListGrantsResponse' {} a -> s {nextMarker = a} :: ListGrantsResponse)

instance Core.FromJSON ListGrantsResponse where
  parseJSON =
    Core.withObject
      "ListGrantsResponse"
      ( \x ->
          ListGrantsResponse'
            Prelude.<$> (x Core..:? "Truncated")
            Prelude.<*> (x Core..:? "Grants" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "NextMarker")
      )

instance Prelude.Hashable ListGrantsResponse where
  hashWithSalt salt' ListGrantsResponse' {..} =
    salt' `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` grants
      `Prelude.hashWithSalt` truncated

instance Prelude.NFData ListGrantsResponse where
  rnf ListGrantsResponse' {..} =
    Prelude.rnf truncated
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf grants

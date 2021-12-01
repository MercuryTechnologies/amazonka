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
-- Module      : Amazonka.CloudDirectory.Types.BatchListObjectParentsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListObjectParentsResponse where

import Amazonka.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a ListObjectParents response operation.
--
-- /See:/ 'newBatchListObjectParentsResponse' smart constructor.
data BatchListObjectParentsResponse = BatchListObjectParentsResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of parent reference and LinkName Tuples.
    parentLinks :: Prelude.Maybe [ObjectIdentifierAndLinkNameTuple]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectParentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListObjectParentsResponse_nextToken' - The pagination token.
--
-- 'parentLinks', 'batchListObjectParentsResponse_parentLinks' - Returns a list of parent reference and LinkName Tuples.
newBatchListObjectParentsResponse ::
  BatchListObjectParentsResponse
newBatchListObjectParentsResponse =
  BatchListObjectParentsResponse'
    { nextToken =
        Prelude.Nothing,
      parentLinks = Prelude.Nothing
    }

-- | The pagination token.
batchListObjectParentsResponse_nextToken :: Lens.Lens' BatchListObjectParentsResponse (Prelude.Maybe Prelude.Text)
batchListObjectParentsResponse_nextToken = Lens.lens (\BatchListObjectParentsResponse' {nextToken} -> nextToken) (\s@BatchListObjectParentsResponse' {} a -> s {nextToken = a} :: BatchListObjectParentsResponse)

-- | Returns a list of parent reference and LinkName Tuples.
batchListObjectParentsResponse_parentLinks :: Lens.Lens' BatchListObjectParentsResponse (Prelude.Maybe [ObjectIdentifierAndLinkNameTuple])
batchListObjectParentsResponse_parentLinks = Lens.lens (\BatchListObjectParentsResponse' {parentLinks} -> parentLinks) (\s@BatchListObjectParentsResponse' {} a -> s {parentLinks = a} :: BatchListObjectParentsResponse) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON BatchListObjectParentsResponse where
  parseJSON =
    Core.withObject
      "BatchListObjectParentsResponse"
      ( \x ->
          BatchListObjectParentsResponse'
            Prelude.<$> (x Core..:? "NextToken")
            Prelude.<*> (x Core..:? "ParentLinks" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchListObjectParentsResponse
  where
  hashWithSalt
    salt'
    BatchListObjectParentsResponse' {..} =
      salt' `Prelude.hashWithSalt` parentLinks
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    BatchListObjectParentsResponse
  where
  rnf BatchListObjectParentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parentLinks

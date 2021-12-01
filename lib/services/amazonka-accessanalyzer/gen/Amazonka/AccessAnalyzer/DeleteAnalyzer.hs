{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AccessAnalyzer.DeleteAnalyzer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified analyzer. When you delete an analyzer, IAM Access
-- Analyzer is disabled for the account or organization in the current or
-- specific Region. All findings that were generated by the analyzer are
-- deleted. You cannot undo this action.
module Amazonka.AccessAnalyzer.DeleteAnalyzer
  ( -- * Creating a Request
    DeleteAnalyzer (..),
    newDeleteAnalyzer,

    -- * Request Lenses
    deleteAnalyzer_clientToken,
    deleteAnalyzer_analyzerName,

    -- * Destructuring the Response
    DeleteAnalyzerResponse (..),
    newDeleteAnalyzerResponse,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes an analyzer.
--
-- /See:/ 'newDeleteAnalyzer' smart constructor.
data DeleteAnalyzer = DeleteAnalyzer'
  { -- | A client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the analyzer to delete.
    analyzerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnalyzer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteAnalyzer_clientToken' - A client token.
--
-- 'analyzerName', 'deleteAnalyzer_analyzerName' - The name of the analyzer to delete.
newDeleteAnalyzer ::
  -- | 'analyzerName'
  Prelude.Text ->
  DeleteAnalyzer
newDeleteAnalyzer pAnalyzerName_ =
  DeleteAnalyzer'
    { clientToken = Prelude.Nothing,
      analyzerName = pAnalyzerName_
    }

-- | A client token.
deleteAnalyzer_clientToken :: Lens.Lens' DeleteAnalyzer (Prelude.Maybe Prelude.Text)
deleteAnalyzer_clientToken = Lens.lens (\DeleteAnalyzer' {clientToken} -> clientToken) (\s@DeleteAnalyzer' {} a -> s {clientToken = a} :: DeleteAnalyzer)

-- | The name of the analyzer to delete.
deleteAnalyzer_analyzerName :: Lens.Lens' DeleteAnalyzer Prelude.Text
deleteAnalyzer_analyzerName = Lens.lens (\DeleteAnalyzer' {analyzerName} -> analyzerName) (\s@DeleteAnalyzer' {} a -> s {analyzerName = a} :: DeleteAnalyzer)

instance Core.AWSRequest DeleteAnalyzer where
  type
    AWSResponse DeleteAnalyzer =
      DeleteAnalyzerResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteAnalyzerResponse'

instance Prelude.Hashable DeleteAnalyzer where
  hashWithSalt salt' DeleteAnalyzer' {..} =
    salt' `Prelude.hashWithSalt` analyzerName
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData DeleteAnalyzer where
  rnf DeleteAnalyzer' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf analyzerName

instance Core.ToHeaders DeleteAnalyzer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteAnalyzer where
  toPath DeleteAnalyzer' {..} =
    Prelude.mconcat
      ["/analyzer/", Core.toBS analyzerName]

instance Core.ToQuery DeleteAnalyzer where
  toQuery DeleteAnalyzer' {..} =
    Prelude.mconcat ["clientToken" Core.=: clientToken]

-- | /See:/ 'newDeleteAnalyzerResponse' smart constructor.
data DeleteAnalyzerResponse = DeleteAnalyzerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnalyzerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAnalyzerResponse ::
  DeleteAnalyzerResponse
newDeleteAnalyzerResponse = DeleteAnalyzerResponse'

instance Prelude.NFData DeleteAnalyzerResponse where
  rnf _ = ()
